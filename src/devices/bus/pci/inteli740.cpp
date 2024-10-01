// license:BSD-3-Clause
// copyright-holders:
/**************************************************************************************************

Intel i740

TODO:
- stub device;
- needs Intel i740 VGA i/f emulator for that

**************************************************************************************************/

#include "emu.h"
#include "inteli740.h"

#define LOG_WARN      (1U << 1)
#define LOG_TODO      (1U << 2) // log unimplemented registers

#define VERBOSE (LOG_GENERAL | LOG_WARN | LOG_TODO)
//#define LOG_OUTPUT_FUNC osd_printf_info
#include "logmacro.h"

#define LOGWARN(...)            LOGMASKED(LOG_WARN, __VA_ARGS__)
#define LOGTODO(...)            LOGMASKED(LOG_TODO, __VA_ARGS__)


DEFINE_DEVICE_TYPE(INTELI740,   inteli740_device,   "inteli740",   "Intel i740")

inteli740_device::inteli740_device(const machine_config &mconfig, device_type type, const char *tag, device_t *owner, uint32_t clock)
	: pci_card_device(mconfig, type, tag, owner, clock)
	, m_svga(*this, "svga")
	, m_vga_rom(*this, "vga_rom")
{
	// Intel i740 AGP
	set_ids_agp(0x80867800, 0x21, 0x00000000);
}

inteli740_device::inteli740_device(const machine_config &mconfig, const char *tag, device_t *owner, uint32_t clock)
	: inteli740_device(mconfig, INTELI740, tag, owner, clock)
{
}

ROM_START( inteli740 )
	ROM_REGION32_LE( 0x7a00, "vga_rom", ROMREGION_ERASEFF )
	ROM_LOAD( "acorp740.vbi", 0x000000, 0x7a00, CRC(0d3887a9) SHA1(d28b9ff90bc74415da21f74ba0d4e011a2e6cf65) )
ROM_END

const tiny_rom_entry *inteli740_device::device_rom_region() const
{
	return ROM_NAME(inteli740);
}

void inteli740_device::device_add_mconfig(machine_config &config)
{
	screen_device &screen(SCREEN(config, "screen", SCREEN_TYPE_RASTER));
	screen.set_raw(XTAL(25'174'800), 900, 0, 640, 526, 0, 480);
	screen.set_screen_update(m_svga, FUNC(vga_device::screen_update));

	// TODO: Intel i740 "VGA Emulator"
	VGA(config, m_svga, 0);
	m_svga->set_screen("screen");
	m_svga->set_vram_size(8*1024*1024);
}



void inteli740_device::device_start()
{
	pci_card_device::device_start();

	add_map(  16*1024*1024, M_MEM | M_PREF, FUNC(inteli740_device::i740_fb_map));
	add_map(  512*1024, M_MEM | M_PREF, FUNC(inteli740_device::i740_mmio_map));

	add_rom((u8 *)m_vga_rom->base(), 64*1024 );
	expansion_rom_base = 0xc0000;

	// INTA#
	intr_pin = 1;
}

void inteli740_device::device_reset()
{
	pci_card_device::device_reset();

	command = 0x0000;
	command_mask = 0x0027;
	// medium DEVSELB + 66mhz + fast back to back + new cap support
	status = 0x02b0;

	remap_cb();
}

u8 inteli740_device::capptr_r()
{
	return 0xd0;
}

void inteli740_device::config_map(address_map &map)
{
	pci_device::config_map(map);

	map(0x34, 0x34).r(FUNC(inteli740_device::capptr_r));

	// AGP
	map(0xd0, 0xd3).lr32(NAME([] { return 0x0010'0002; }));
	// AGP 1x & 2x + SBA + 31 RQ
	map(0xd4, 0xd7).lr32(NAME([] { return 0x1f00'0203; }));

	// Power management
	map(0xdc, 0xdf).lr32(NAME([] { return 0x0201'0001; }));
}



void inteli740_device::i740_fb_map(address_map &map)
{

}

void inteli740_device::i740_mmio_map(address_map &map)
{
}

// TODO: this should really be a subclass of VGA
void inteli740_device::legacy_memory_map(address_map &map)
{
	map(0xa0000, 0xbffff).rw(FUNC(inteli740_device::vram_r), FUNC(inteli740_device::vram_w));
}

void inteli740_device::legacy_io_map(address_map &map)
{
	map(0, 0x02f).m(m_svga, FUNC(vga_device::io_map));
}

uint8_t inteli740_device::vram_r(offs_t offset)
{
	return downcast<vga_device *>(m_svga.target())->mem_r(offset);
}

void inteli740_device::vram_w(offs_t offset, uint8_t data)
{
	downcast<vga_device *>(m_svga.target())->mem_w(offset, data);
}

void inteli740_device::map_extra(uint64_t memory_window_start, uint64_t memory_window_end, uint64_t memory_offset, address_space *memory_space,
							uint64_t io_window_start, uint64_t io_window_end, uint64_t io_offset, address_space *io_space)
{
	if (command & 7)
	{
		memory_space->install_readwrite_handler(0xa0000, 0xbffff, read8sm_delegate(*this, FUNC(inteli740_device::vram_r)), write8sm_delegate(*this, FUNC(inteli740_device::vram_w)));

		io_space->install_device(0x03b0, 0x03df, *this, &inteli740_device::legacy_io_map);
		//memory_space->install_rom(0xc0000, 0xcffff, (void *)expansion_rom);
	}
}
