/*
 * ASPEED Hash and Crypto Engine
 *
 * Copyright (C) 2021 IBM Corp.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/log.h"
#include "qemu/error-report.h"
#include "hw/misc/aspeed_hace.h"
#include "qapi/error.h"
#include "migration/vmstate.h"
#include "crypto/hash.h"
#include "hw/qdev-properties.h"

#define R_STATUS        (0x1c / 4)
#define HASH_IRQ        BIT(9)
#define CRYPT_IRQ       BIT(12)
#define TAG_IRQ         BIT(15)

#define R_HASH_CMD      (0x30 / 4)
#define  HASH_ALGO_MASK                 (BIT(4) | BIT(5) | BIT(6))
#define  HASH_ALGO_MD5                  0
#define  HASH_ALGO_SHA1                 BIT(5)
#define  HASH_ALGO_SHA224               BIT(6)
#define  HASH_ALGO_SHA256               (BIT(4) | BIT(6))
#define  HASH_ALGO_SHA512_SERIES        (BIT(5) | BIT(6))
#define  SHA512_HASH_ALGO_MASK          (BIT(10) | BIT(11) | BIT(12))
#define  HASH_ALGO_SHA512_SHA512        0
#define  HASH_ALGO_SHA512_SHA384        BIT(10)
#define  HASH_ALGO_SHA512_SHA256        BIT(11)
#define  HASH_ALGO_SHA512_SHA224        (BIT(10) | BIT(11))

#define R_HASH_SRC              (0x20 / 4)
#define R_HASH_DEST             (0x24 / 4)
#define R_HASH_SRC_LEN          (0x2c / 4)

static int do_hash_operation(AspeedHACEState *s, int algo) {
    hwaddr src, len, dest;
    uint8_t *digest_buf = NULL;
    size_t digest_len = 0;
    char *src_buf;
    int rc;

    src = s->regs[R_HASH_SRC];
    len = s->regs[R_HASH_SRC_LEN];
    dest = s->regs[R_HASH_DEST];

    src_buf = address_space_map(&s->dram_as, src, &len, false,
                                MEMTXATTRS_UNSPECIFIED);
    if (!src_buf) {
        qemu_log_mask(LOG_GUEST_ERROR, "%s: failed to map dram\n", __func__);
        return -EACCES;
    }

    rc = qcrypto_hash_bytes(algo, src_buf, len, &digest_buf, &digest_len,
                            &error_fatal);
    if (rc < 0) {
        qemu_log_mask(LOG_GUEST_ERROR, "%s: qcrypto failed\n", __func__);
        return rc;
    }

    rc = address_space_write(&s->dram_as, dest, MEMTXATTRS_UNSPECIFIED,
                             digest_buf, digest_len);
    if (rc) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "%s: addresss space write failed\n", __func__);
    }
    g_free(digest_buf);

    address_space_unmap(&s->dram_as, src_buf, len, false, len);

    /* Set irq to indicate completion */
    s->regs[R_STATUS] |= HASH_IRQ;

    return 0;
}


static uint64_t aspeed_hace_read(void *opaque, hwaddr addr, unsigned int size)
{
    AspeedHACEState *s = ASPEED_HACE(opaque);

    addr >>= 2;

    if (addr >= ASPEED_HACE_NR_REGS) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "%s: Out-of-bounds read at offset 0x%" HWADDR_PRIx "\n",
                      __func__, addr << 2);
        return 0;
    }

    switch (addr) {
    case R_STATUS:
        break;
    default:
        break;
    }

    return s->regs[addr];
}

static void aspeed_hace_write(void *opaque, hwaddr addr, uint64_t data,
                              unsigned int size)
{
    AspeedHACEState *s = ASPEED_HACE(opaque);

    addr >>= 2;

    if (addr >= ASPEED_HACE_NR_REGS) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "%s: Out-of-bounds write at offset 0x%" HWADDR_PRIx "\n",
                      __func__, addr << 2);
        return;
    }

    switch (addr) {
    case R_STATUS:
        data &= ~(HASH_IRQ | CRYPT_IRQ | TAG_IRQ);
        break;
    case R_HASH_CMD: {
        int algo = -1;
        switch (data & HASH_ALGO_MASK) {
        case HASH_ALGO_MD5:
            algo = QCRYPTO_HASH_ALG_MD5;
            break;
        case HASH_ALGO_SHA1:
            algo = QCRYPTO_HASH_ALG_SHA1;
            break;
        case HASH_ALGO_SHA224:
            algo = QCRYPTO_HASH_ALG_SHA224;
            break;
        case HASH_ALGO_SHA256:
            algo = QCRYPTO_HASH_ALG_SHA256;
            break;
        case HASH_ALGO_SHA512_SERIES:
            switch (data & SHA512_HASH_ALGO_MASK) {
                case HASH_ALGO_SHA512_SHA512:
                    algo = QCRYPTO_HASH_ALG_SHA512;
                    break;
                case HASH_ALGO_SHA512_SHA384:
                    algo = QCRYPTO_HASH_ALG_SHA384;
                    break;
                case HASH_ALGO_SHA512_SHA256:
                    algo = QCRYPTO_HASH_ALG_SHA256;
                    break;
                case HASH_ALGO_SHA512_SHA224:
                    algo = QCRYPTO_HASH_ALG_SHA224;
                    break;
                default:
                    qemu_log_mask(LOG_GUEST_ERROR,
                            "%s: Invalid sha512 hash algorithim selection 0x%03lx\n",
                            __func__, data & SHA512_HASH_ALGO_MASK);
                    break;
            }
            break;
        default:
            qemu_log_mask(LOG_GUEST_ERROR,
                      "%s: Invalid hash algorithim selection 0x%03lx\n",
                      __func__, data & HASH_ALGO_MASK);
            break;
        }
        if (algo >= 0) {
            do_hash_operation(s, algo);
        }

        break;
    }
    default:
        break;
    }

    s->regs[addr] = data;
}

static const MemoryRegionOps aspeed_hace_ops = {
    .read = aspeed_hace_read,
    .write = aspeed_hace_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 4,
    },
};

static void aspeed_hace_reset(DeviceState *dev)
{
    struct AspeedHACEState *s = ASPEED_HACE(dev);

    memset(s->regs, 0, sizeof(s->regs));
}

static void aspeed_hace_realize(DeviceState *dev, Error **errp)
{
    AspeedHACEState *s = ASPEED_HACE(dev);
    SysBusDevice *sbd = SYS_BUS_DEVICE(dev);

    sysbus_init_irq(sbd, &s->irq);

    memory_region_init_io(&s->iomem, OBJECT(s), &aspeed_hace_ops, s,
            TYPE_ASPEED_HACE, 0x1000);

    if (!s->dram_mr) {
        error_setg(errp, TYPE_ASPEED_HACE ": 'dram' link not set");
        return;
    }

    address_space_init(&s->dram_as, s->dram_mr, "dram");

    sysbus_init_mmio(sbd, &s->iomem);
}

static Property aspeed_hace_properties[] = {
    DEFINE_PROP_LINK("dram", AspeedHACEState, dram_mr,
                     TYPE_MEMORY_REGION, MemoryRegion *),
    DEFINE_PROP_END_OF_LIST(),
};


static const VMStateDescription vmstate_aspeed_hace = {
    .name = TYPE_ASPEED_HACE,
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (VMStateField[]) {
        VMSTATE_UINT32_ARRAY(regs, AspeedHACEState, ASPEED_HACE_NR_REGS),
        VMSTATE_END_OF_LIST(),
    }
};

static void aspeed_hace_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);

    dc->realize = aspeed_hace_realize;
    dc->reset = aspeed_hace_reset;
    device_class_set_props(dc, aspeed_hace_properties);
    dc->desc = "Aspeed Hash and Crytpo Engine",
    dc->vmsd = &vmstate_aspeed_hace;
}

static const TypeInfo aspeed_hace_info = {
    .name = TYPE_ASPEED_HACE,
    .parent = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(AspeedHACEState),
    .class_init = aspeed_hace_class_init,
};

static void aspeed_hace_register_types(void)
{
    type_register_static(&aspeed_hace_info);
}

type_init(aspeed_hace_register_types);
