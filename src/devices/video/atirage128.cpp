// license:BSD-3-Clause
// copyright-holders:
/**************************************************************************************************

    ATI Rage 128 PCI/AGP SVGA

    Reference: "ATI Rage 128 Register Reference Guide Technical Reference Manual PN RRG-G04100-C (1998-11) v0.01.pdf", aka "Rage 128 Register Reference Guide Rev 0.01"
    http://hackipedia.org/browse.cgi/Computer/Platform/PC%2C%20IBM%20compatible/Video/VGA/SVGA/ATI%2C%20Array%20Technology%20Inc

**************************************************************************************************/

#include "emu.h"
#include "screen.h"
#include "atirage128.h"

#define LOG_REGISTERS   (1U << 1)
#define LOG_CRTC        (1U << 2)
#define LOG_DAC         (1U << 3)

#define VERBOSE (0)
#include "logmacro.h"

DEFINE_DEVICE_TYPE(ATI_RAGE128PRO_X86, atirage128pro_x86_device, "atirage128pro_x86", "ATI Rage 128 Pro (x86 PCI Device)")

// register offsets
static constexpr u32 CRTC_H_TOTAL_DISP  = 0x0200;
static constexpr u32 CRTC_V_TOTAL_DISP  = 0x0208;
static constexpr u32 CRTC_OFFSET        = 0x0224;
static constexpr u32 CRTC_GEN_CNTL      = 0x0050;
static constexpr u32 GPIO_MONID         = 0x0068;
static constexpr u32 CLOCK_CNTL_INDEX   = 0x0008;
static constexpr u32 CLOCK_CNTL_DATA    = 0x000c;
static constexpr u32 DAC_CNTL      		= 0x0058;

// PLL register offsets
static constexpr u32 PLL_MACRO_CNTL     = 1;
static constexpr u32 PLL_REF_DIV        = 2;
static constexpr u32 PLL_GEN_CNTL       = 3;
static constexpr u32 MCLK_FB_DIV        = 4;
static constexpr u32 PLL_VCLK_CNTL      = 5;
static constexpr u32 VCLK_POST_DIV      = 6;
static constexpr u32 VCLK0_FB_DIV       = 7;
static constexpr u32 VCLK1_FB_DIV       = 8;
static constexpr u32 VCLK2_FB_DIV       = 9;
static constexpr u32 VCLK3_FB_DIV       = 10;
static constexpr u32 PLL_XCLK_CNTL      = 11;
static constexpr u32 PLL_FCP_CNTL       = 12;

// mach64 & 3D Rage post-dividers for PLL
static const int pll_post_dividers[8] =
{
	1, 2, 4, 8, 3, 5, 6, 12
};

void atirage128_device::device_add_mconfig(machine_config &config)
{
	screen_device &screen(SCREEN(config, "screen", SCREEN_TYPE_RASTER));
	screen.set_raw(XTAL(25'174'800), 900, 0, 640, 526, 0, 480);
	screen.set_screen_update(FUNC(atirage128_device::screen_update));

	ATIMACH64(config, m_mach64, 0);
	m_mach64->set_screen("screen");
	m_mach64->set_vram_size(16*1024*1024);
}

atirage128_device::atirage128_device(const machine_config &mconfig, device_type type, const char *tag, device_t *owner, uint32_t clock)
	: pci_device(mconfig, type, tag, owner, clock),
	m_mach64(*this, "vga"),
	m_screen(*this, "screen"),
	read_gpio(*this, 0),
	write_gpio(*this)
{
	m_hres = m_vres = m_htotal = m_vtotal = m_format = 0;
	m_dac_windex = m_dac_rindex = m_dac_state = 0;
	m_dac_mask = 0xff;

}

atirage128pro_x86_device::atirage128pro_x86_device(const machine_config &mconfig, const char *tag, device_t *owner, uint32_t clock)
	: atirage128_device(mconfig, ATI_RAGE128PRO_X86, tag, owner, clock)
	, m_vga_rom(*this, "vga_rom")
{
}

void atirage128_device::io_map(address_map& map)
{
	map(0x00000000, 0x000003ff).rw(FUNC(atirage128_device::regs_0_read), FUNC(atirage128_device::regs_0_write));
}

void atirage128_device::mem_map(address_map& map)
{
	map(0x00000000, 0x005fffff).rw(m_mach64, FUNC(mach64_device::framebuffer_r), FUNC(mach64_device::framebuffer_w));
	map(0x007ff800, 0x007ffbff).rw(FUNC(atirage128_device::regs_1_read), FUNC(atirage128_device::regs_1_write));
	map(0x007ffc00, 0x007fffff).rw(FUNC(atirage128_device::regs_0_read), FUNC(atirage128_device::regs_0_write));
	map(0x01000000, 0x01ffffff).rw(m_mach64, FUNC(mach64_device::framebuffer_be_r), FUNC(mach64_device::framebuffer_be_w));
}

void atirage128_device::reg_map(address_map& map)
{
}

void atirage128_device::config_map(address_map &map)
{
	pci_device::config_map(map);
	map(0x0040, 0x0043).rw(FUNC(atirage128_device::user_cfg_r), FUNC(atirage128_device::user_cfg_w));
}

void atirage128_device::device_start()
{
	pci_device::device_start();

	add_map(64*1024*1024, M_MEM | M_PREF, FUNC(atirage128_device::mem_map));   // 64 MB memory map
	add_map(0x100, M_IO, FUNC(atirage128_device::io_map));     // 256 byte I/O map
	add_map(16*1024, M_MEM, FUNC(atirage128_device::reg_map)); // 16K register map

	command = 3;
	intr_pin = 1;
	intr_line = 0;

	// clear the registers
	std::fill(std::begin(m_regs0), std::end(m_regs0), 0);
	std::fill(std::begin(m_regs1), std::end(m_regs1), 0);
	std::fill(std::begin(m_pll_regs), std::end(m_pll_regs), 0);
	std::fill(std::begin(m_dac_colors), std::end(m_dac_colors), 0);

	// set PLL defaults from the manual
	m_pll_regs[PLL_MACRO_CNTL] = 0xd4;
	m_pll_regs[PLL_REF_DIV] = 0x36;
	m_pll_regs[PLL_GEN_CNTL] = 0x4f;
	m_pll_regs[MCLK_FB_DIV] = 0x97;
	m_pll_regs[PLL_VCLK_CNTL] = 0x04;
	m_pll_regs[VCLK_POST_DIV] = 0x6a;
	m_pll_regs[VCLK0_FB_DIV] = 0xbe;
	m_pll_regs[VCLK1_FB_DIV] = 0xd6;
	m_pll_regs[VCLK2_FB_DIV] = 0xee;
	m_pll_regs[VCLK3_FB_DIV] = 0x88;
	m_pll_regs[PLL_XCLK_CNTL] = 0x00;
	m_pll_regs[PLL_FCP_CNTL] = 0x41;

	m_user_cfg = 8;
	save_item(NAME(m_user_cfg));
	save_item(NAME(m_regs0));
	save_item(NAME(m_regs1));
	save_item(NAME(m_pll_regs));
	save_item(NAME(m_hres));
	save_item(NAME(m_vres));
	save_item(NAME(m_htotal));
	save_item(NAME(m_vtotal));
	save_item(NAME(m_format));
	save_item(NAME(m_pixel_clock));
	save_item(NAME(m_dac_windex));
	save_item(NAME(m_dac_rindex));
	save_item(NAME(m_dac_state));
	save_item(NAME(m_dac_mask));
	save_item(NAME(m_dac_colors));
}

void atirage128pro_x86_device::device_start()
{
	// Rage 128 Pro
	set_ids(0x10025446, 0x00, 0x030000, 0x10020409);
	atirage128_device::device_start();

	command = 0x0000;
	command_mask = 0x0027;
	status = 0x02b0;

	add_rom((u8 *)m_vga_rom->base(), 0x8000);
	expansion_rom_base = 0xc0000;
}

void atirage128pro_x86_device::device_reset()
{
	atirage128_device::device_reset();
	remap_cb();
}

ROM_START( atirage128pro )
	ROM_REGION32_LE( 0x8000, "vga_rom", ROMREGION_ERASEFF )
	ROM_SYSTEM_BIOS( 0, "rage128pro16mb", "ATI Rage 128 Pro 16MB" )
	ROMX_LOAD( "rage128pro16mb.vbi", 0x0000, 0x8000, CRC(dec42402) SHA1(afd3a059857529aa099158ea1ddbec5add1a0790), ROM_BIOS(0) )
ROM_END

const tiny_rom_entry *atirage128pro_x86_device::device_rom_region() const
{
	return ROM_NAME(atirage128pro);
}

void atirage128pro_x86_device::config_map(address_map &map)
{
	atirage128_device::config_map(map);

	map(0x34, 0x34).lr8(NAME([] { return 0x50; }));

	map(0x50, 0x53).lr32(NAME([] { return 0x0020'5c02; }));

	map(0x54, 0x57).lr32(NAME([] { return 0x1f00'0203; }));

	map(0x5c, 0x5e).lr32(NAME([] { return 0x0002'0001; }));
}

uint8_t atirage128pro_x86_device::vram_r(offs_t offset)
{
	return downcast<mach64_device *>(m_mach64.target())->mem_r(offset);
}

void atirage128pro_x86_device::vram_w(offs_t offset, uint8_t data)
{
	downcast<mach64_device *>(m_mach64.target())->mem_w(offset, data);
}

void atirage128pro_x86_device::legacy_io_map(address_map &map)
{
	map(0x0000, 0x02f).m(m_mach64, FUNC(mach64_device::io_map));
}

void atirage128pro_x86_device::map_extra(uint64_t memory_window_start, uint64_t memory_window_end, uint64_t memory_offset, address_space *memory_space,
							uint64_t io_window_start, uint64_t io_window_end, uint64_t io_offset, address_space *io_space)
{
	if (BIT(command, 1))
	{
		memory_space->install_readwrite_handler(0xa0000, 0xbffff, read8sm_delegate(*this, FUNC(atirage128pro_x86_device::vram_r)), write8sm_delegate(*this, FUNC(atirage128pro_x86_device::vram_w)));
	}

	if (BIT(command, 0))
	{
		io_space->install_device(0x03b0, 0x03df, *this, &atirage128pro_x86_device::legacy_io_map);
		io_space->install_readwrite_handler(0x01ce, 0x01cf, read8sm_delegate(*m_mach64, FUNC(mach64_device::ati_port_ext_r)), write8sm_delegate(*m_mach64, FUNC(mach64_device::ati_port_ext_w)));
	}
}

void atirage128_device::map_extra(uint64_t memory_window_start, uint64_t memory_window_end, uint64_t memory_offset, address_space *memory_space,
							uint64_t io_window_start, uint64_t io_window_end, uint64_t io_offset, address_space *io_space)
{
}

u8 atirage128_device::regs_0_read(offs_t offset)
{
	switch (offset)
	{
		case DAC_CNTL: // DAC write index
			return m_dac_windex;

		case DAC_CNTL + 1:
			{
				u8 result = 0;
				switch (m_dac_state)
				{
					case 0: // red
						result = ((m_dac_colors[m_dac_rindex] >> 16) & 0xff);
						break;

					case 1: // blue
						result = ((m_dac_colors[m_dac_rindex] >> 8) & 0xff);
						break;

					case 2: // green
						result = (m_dac_colors[m_dac_rindex] & 0xff);
						break;
				}

				m_dac_state++;
				if (m_dac_state >= 3)
				{
					m_dac_state = 0;
					m_dac_rindex++;
				}
				return result;
			}
			break;

		case DAC_CNTL + 2:
			return m_dac_mask;

		case DAC_CNTL + 3:
			return m_dac_rindex;

		case CLOCK_CNTL_INDEX + 2:
		case CLOCK_CNTL_DATA:
			return m_pll_regs[(m_regs0[CLOCK_CNTL_INDEX+1] >> 2) & 0xf] << 16;
	}

	return m_regs0[offset];
}

void atirage128_device::regs_0_write(offs_t offset, u8 data)
{
	LOGMASKED(LOG_REGISTERS, "regs_0_write: %02x to %x\n", data, offset);

	m_regs0[offset] = data;
	switch (offset)
	{
		case DAC_CNTL: // DAC write index
				m_dac_state = 0;
				m_dac_windex = data;
				break;

		case DAC_CNTL + 1:
			switch (m_dac_state)
			{
				case 0: // red
					m_dac_colors[m_dac_windex] &= 0x00ffff;
					m_dac_colors[m_dac_windex] |= ((data & 0xff) << 16);
					break;

				case 1: // green
					m_dac_colors[m_dac_windex] &= 0xff00ff;
					m_dac_colors[m_dac_windex] |= ((data & 0xff) << 8);
					break;

				case 2: // blue
					m_dac_colors[m_dac_windex] &= 0xffff00;
					m_dac_colors[m_dac_windex] |= (data & 0xff);
					break;
			}

			m_dac_state++;
			if (m_dac_state == 3)
			{
				m_dac_state = 0;
				m_mach64->set_color(m_dac_windex, m_dac_colors[m_dac_windex]);
				m_dac_windex++;
			}
			break;

		case DAC_CNTL + 2:
			m_dac_mask = data;
			break;

		case DAC_CNTL + 3:
			m_dac_state = 0;
			m_dac_rindex = data;
			break;

		case CRTC_OFFSET:
		case CRTC_GEN_CNTL:
			update_mode();
			break;

		case CLOCK_CNTL_INDEX + 2:
		case CLOCK_CNTL_DATA:
			if (BIT(m_regs0[CLOCK_CNTL_INDEX+1], 1))
			{
				u8 regnum = (m_regs0[CLOCK_CNTL_INDEX+1] >> 2) & 0xf;
				m_pll_regs[regnum] = data & 0xff;
			}
			break;

		case GPIO_MONID:
		case GPIO_MONID + 1:
		case GPIO_MONID + 2:
		case GPIO_MONID + 3:
			{
				u16 old_data = *(u16 *)&m_regs0[GPIO_MONID];
				const u16 ddr = *(u16 *)&m_regs0[GPIO_MONID+2];

				old_data &= ddr;                // 0 bits are input

				// send the data to an external handler
				// AND the pullups by the inverse of DDR, so bits set to input get the pullup
				write_gpio(old_data | (m_gpio_pullups & (ddr ^ 0xffff)));

				// get the updated data from the port
				u16 new_data = read_gpio();
				new_data &= (ddr ^ 0xffff);     // AND against inverted DDR mask so 0 bits are output
				new_data |= old_data;
				m_regs0[GPIO_MONID] = (new_data & 0xff);
				m_regs0[GPIO_MONID + 1] = (new_data >> 8) & 0xff;
			}
			break;
	}
}

u8 atirage128_device::regs_1_read(offs_t offset)
{
	LOGMASKED(LOG_REGISTERS, "regs 1 read @ %x\n", offset);
	return m_regs1[offset];
}

void atirage128_device::regs_1_write(offs_t offset, u8 data)
{
	m_regs1[offset] = data;
}

u32 atirage128_device::user_cfg_r()
{
	return m_user_cfg;
}

void atirage128_device::user_cfg_w(u32 data)
{
	m_user_cfg = data;
}

void atirage128_device::update_mode()
{
	// first prereq: must be in native mode and the CRTC must be enabled
	if (!(m_regs0[CRTC_GEN_CNTL+3] & 3))
	{
		LOGMASKED(LOG_CRTC, "VGA mode must be OFF and CRTC must be ON\n");
		return;
	}

	m_htotal = (m_regs0[CRTC_H_TOTAL_DISP] | (m_regs0[CRTC_H_TOTAL_DISP+1] & 1) << 8) + 1;
	m_htotal <<= 3; // in units of 8 pixels
	m_hres = m_regs0[CRTC_H_TOTAL_DISP+2] + 1;
	m_hres <<= 3;
	m_vres = (m_regs0[CRTC_V_TOTAL_DISP+2] | (m_regs0[CRTC_V_TOTAL_DISP+3] & 7) << 8) + 1;
	m_vtotal = (m_regs0[CRTC_V_TOTAL_DISP] | (m_regs0[CRTC_V_TOTAL_DISP+1] & 7) << 8) + 1;
	m_format = m_regs0[CRTC_GEN_CNTL+1] & 7;
	LOGMASKED(LOG_CRTC, "Setting mode (%d x %d), total (%d x %d) format %d\n", m_hres, m_vres, m_htotal, m_vtotal, m_format);

	double vpll_frequency;
	int clk_source = m_regs0[CLOCK_CNTL_INDEX] & 3;

	switch (m_pll_regs[PLL_VCLK_CNTL] & 3)
	{
		case 0: // CPUCLK (the PCI bus clock, not to exceed 33 MHz)
			vpll_frequency = (33000000.0 * m_pll_regs[VCLK0_FB_DIV + clk_source]) / m_pll_regs[PLL_REF_DIV];
			break;

		case 3: // PLLVCLK
			vpll_frequency = ((clock() * 2.0) * m_pll_regs[VCLK0_FB_DIV + clk_source]) / m_pll_regs[PLL_REF_DIV];
			break;

		default:
			LOGMASKED(LOG_CRTC, "VCLK source (%d) is not VPLL, can't calculate dot clock\n", m_pll_regs[PLL_VCLK_CNTL] & 3);
			return;
	}
	LOGMASKED(LOG_CRTC, "VPLL freq %f\n", vpll_frequency);

	int vpll_post_divider = (m_pll_regs[VCLK_POST_DIV] >> (clk_source << 1)) & 3;
	// Rage Pro adds one more bit to the divider from bits 4/5/6/7 of XCLK_CNTL depending on the clock source.
	// This should always be zero on mach64/Rage/Rage II.
	vpll_post_divider |= ((m_pll_regs[PLL_XCLK_CNTL] >> (clk_source + 2)) & 4);

	m_pixel_clock = u32(vpll_frequency / pll_post_dividers[vpll_post_divider]);
	LOGMASKED(LOG_CRTC, "Pixel clock = %d, refresh = %f\n", m_pixel_clock, (double)m_pixel_clock / (double)m_htotal / (double)m_vtotal);

	rectangle visarea(0, m_hres - 1, 0, m_vres - 1);
	m_screen->configure(m_htotal, m_vtotal, visarea, attotime::from_ticks(m_htotal * m_vtotal, m_pixel_clock).as_attoseconds());
}

u32 atirage128_device::screen_update(screen_device &screen, bitmap_rgb32 &bitmap, const rectangle &cliprect)
{
	// are we in VGA mode rather than native?  if so, let the legacy VGA stuff draw.
	if (!(m_regs0[CRTC_GEN_CNTL+3] & 1))
	{
		return m_mach64->screen_update(screen, bitmap, cliprect);
	}

	// is the CRTC not enabled or the display disable bit set?
	if ((!(m_regs0[CRTC_GEN_CNTL+3] & 2)) || (m_regs0[CRTC_GEN_CNTL] & 0x40))
	{
		bitmap.fill(0, cliprect);
		return 0;
	}

	const int offset = ((m_regs0[CRTC_OFFSET+2] & 0xf) << 16) | (m_regs0[CRTC_OFFSET+1] << 8) | (m_regs0[CRTC_OFFSET]);
	u8 *vram = m_mach64->get_framebuffer_addr() + (offset * 8);
	int stride = (m_regs0[CRTC_OFFSET+2] >> 6) | (m_regs0[CRTC_OFFSET+3] << 2);
	stride *= 4;

	switch (m_format)
	{
		case 2: // 8 bpp (also can be a weird 2/2/3 direct color mode)
			for (u32 y = 0; y < m_vres; y++)
			{
				const u8 *src = &vram[stride*y];
				u32 *dst = &bitmap.pix(y, 0);
				for (u32 x = 0; x < m_hres; x++)
				{
					*dst++ = m_dac_colors[src[x]];
				}
				vram += stride;
			}
			break;

		default:
			LOG("Unknown pixel format %d\n", m_format);
			break;
	}

	return 0;
}

