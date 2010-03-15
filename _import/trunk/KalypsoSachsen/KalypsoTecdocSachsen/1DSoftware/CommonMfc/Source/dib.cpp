#include "stdafx.h"
#include "dib.h"

// WIDTHBYTES performs DWORD-aligning of DIB scanlines.  The "bits"
// parameter is the bit count for the scanline (biWidth * biBitCount),
// and this macro returns the number of DWORD-aligned bytes needed
// to hold those bits.

#define WIDTHBYTES(bits)    (((bits) + 31) / 32 * 4)

// Determines the number of colors in the DIB by looking at
// the BitCount filed in the info block.
WORD DibNumColors (VOID FAR * pv)
{
    INT                 bits;
    LPBITMAPINFOHEADER  lpbi;
    LPBITMAPCOREHEADER  lpbc;

    lpbi = ((LPBITMAPINFOHEADER)pv);
    lpbc = ((LPBITMAPCOREHEADER)pv);

    // With the BITMAPINFO format headers, the size of the palette
    // is in biClrUsed, whereas in the BITMAPCORE - style headers, it
    // is dependent on the bits per pixel ( = 2 raised to the power of
    // bits/pixel).
    if (lpbi->biSize != sizeof(BITMAPCOREHEADER))
	{
        if (lpbi->biClrUsed != 0)
            return (WORD)lpbi->biClrUsed;
        bits = lpbi->biBitCount;
    }
    else
        bits = lpbc->bcBitCount;

    switch (bits)
	{
        case 1:
                return 2;
        case 4:
                return 16;
        case 8:
                return 256;
        default:
                // A 24 bitcount DIB has no color table
                return 0;
    }
}

// Calculates the palette size in bytes. If the info. block
// is of the BITMAPCOREHEADER type, the number of colors is
// multiplied by 3 to give the palette size, otherwise the
// number of colors is multiplied by 4.
WORD PaletteSize (VOID FAR * pv)
{
    LPBITMAPINFOHEADER lpbi;
    WORD               NumColors;

    lpbi      = (LPBITMAPINFOHEADER)pv;
    NumColors = DibNumColors(lpbi);

    if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
        return (WORD)(NumColors * sizeof(RGBTRIPLE));
    else
        return (WORD)(NumColors * sizeof(RGBQUAD));
}

// Will create a global memory block in DIB format that
// represents the Device-dependent bitmap (DDB) passed in.
// The palettes hpal1 and hpal2 define a color conversion map. They must have
// the same number of colors. For this to work the DIB must have a color table.
// i.e. biBits must be 8 or less.
HANDLE DibFromBitmap(HBITMAP hbm, DWORD biStyle, WORD biBits, HPALETTE hpal1, HPALETTE hpal2)
{
    BITMAP               bm;
    BITMAPINFOHEADER     bi;
    BITMAPINFOHEADER FAR *lpbi;
    DWORD                dwLen;
    HANDLE               hdib;
    HANDLE               h;
    HDC                  hdc;
	HPALETTE			 hpal;

    if (!hbm)
        return NULL;

    hpal = (HPALETTE)::GetStockObject(DEFAULT_PALETTE);

    ::GetObject(hbm,sizeof(bm),(LPSTR)&bm);

    if (biBits == 0)
        biBits =  bm.bmPlanes * bm.bmBitsPixel;

    bi.biSize               = sizeof(BITMAPINFOHEADER);
    bi.biWidth              = bm.bmWidth;
    bi.biHeight             = bm.bmHeight;
    bi.biPlanes             = 1;
    bi.biBitCount           = biBits;
    bi.biCompression        = biStyle;
    bi.biSizeImage          = 0;
    bi.biXPelsPerMeter      = 0;
    bi.biYPelsPerMeter      = 0;
    bi.biClrUsed            = 0;
    bi.biClrImportant       = 0;

    dwLen  = bi.biSize + PaletteSize(&bi);

    hdc = ::GetDC(NULL);
    hpal = ::SelectPalette(hdc,hpal,FALSE);
	::RealizePalette(hdc);

    hdib = ::GlobalAlloc(GHND,dwLen);

    if (!hdib)
	{
        ::SelectPalette(hdc,hpal,FALSE);
        ::ReleaseDC(NULL,hdc);
        return NULL;
    }

    lpbi = (LPBITMAPINFOHEADER)::GlobalLock(hdib);

    *lpbi = bi;

    // call GetDIBits with a NULL lpBits param, so it will calculate the
    // biSizeImage field for us
    ::GetDIBits(hdc, hbm, 0L, (DWORD)bi.biHeight,
        (LPBYTE)NULL, (LPBITMAPINFO)lpbi, (DWORD)DIB_RGB_COLORS);

    bi = *lpbi;
    ::GlobalUnlock(hdib);

    // If the driver did not fill in the biSizeImage field, make one up
    if (bi.biSizeImage == 0)
	{
        bi.biSizeImage = WIDTHBYTES((DWORD)bm.bmWidth * biBits) * bm.bmHeight;

        if (biStyle != BI_RGB)
            bi.biSizeImage = (bi.biSizeImage * 3) / 2;
    }

    // realloc the buffer big enough to hold all the bits
    dwLen = bi.biSize + PaletteSize(&bi) + bi.biSizeImage;
    if (h = ::GlobalReAlloc(hdib,dwLen,0))
        hdib = h;
    else
	{
        ::GlobalFree(hdib);
        hdib = NULL;

        ::SelectPalette(hdc,hpal,FALSE);
        ::ReleaseDC(NULL,hdc);
        return hdib;
    }

    // call GetDIBits with a NON-NULL lpBits param, and actualy get the
    // bits this time
    lpbi = (LPBITMAPINFOHEADER)::GlobalLock(hdib);

    if (::GetDIBits(hdc,
					hbm,
					0L,
					(DWORD)bi.biHeight,
					(LPBYTE)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi),
					(LPBITMAPINFO)lpbi, (DWORD)DIB_RGB_COLORS) == 0)
	{
		::GlobalUnlock(hdib);
         hdib = NULL;
         ::SelectPalette(hdc,hpal,FALSE);
         ::ReleaseDC(NULL,hdc);
         return NULL;
    }

    bi = *lpbi;

	// Reset the color table for the DIB using the color conversion map
	if (biBits<=8)
	{
		LPBITMAPINFO lpbmi = (LPBITMAPINFO)lpbi;
		LPPALETTEENTRY lppe1, lppe2;
		UINT ps1, ps2, i, j;
		ps1 = ::GetPaletteEntries(hpal1, 0, 0, NULL);
		ps2 = ::GetPaletteEntries(hpal2, 0, 0, NULL);
		if (ps1==ps2)
		{
			lppe1 = (LPPALETTEENTRY)::GlobalAlloc(GMEM_MOVEABLE, sizeof(PALETTEENTRY)*ps1);
			lppe2 = (LPPALETTEENTRY)::GlobalAlloc(GMEM_MOVEABLE, sizeof(PALETTEENTRY)*ps2);
			lppe1 = (LPPALETTEENTRY)::GlobalLock(lppe1);
			lppe2 = (LPPALETTEENTRY)::GlobalLock(lppe2);
			::GetPaletteEntries(hpal1, 0, ps1, lppe1);
			::GetPaletteEntries(hpal2, 0, ps2, lppe2);
			for (i=0; i<ps1; i++)
			{
				COLORREF cr1, cr2;
				
				cr1 = RGB(lppe1[i].peRed, lppe1[i].peGreen, lppe1[i].peBlue);
				for (j=0; j<DibNumColors(lpbi); j++)
				{
					cr2 = RGB(lpbmi->bmiColors[j].rgbRed,
						lpbmi->bmiColors[j].rgbGreen,
						lpbmi->bmiColors[j].rgbBlue);
					if (cr1==cr2)
					{
						lpbmi->bmiColors[j].rgbRed = lppe2[i].peRed;
						lpbmi->bmiColors[j].rgbGreen = lppe2[i].peGreen;
						lpbmi->bmiColors[j].rgbBlue = lppe2[i].peBlue;
					}
				}
			}
			::GlobalUnlock(lppe1);
			::GlobalUnlock(lppe2);
			::GlobalFree(lppe1);
			::GlobalFree(lppe2);
		}
	}
    ::GlobalUnlock(hdib);

    ::SelectPalette(hdc,hpal,FALSE);
    ::ReleaseDC(NULL,hdc);
    return hdib;
}

// Will create a DDB (Device Dependent Bitmap) given a global
// handle to a memory block in CF_DIB format
HBITMAP BitmapFromDib(HANDLE hdib)
{
    LPBITMAPINFOHEADER  lpbi;
    HPALETTE            hpalT;
    HDC                 hdc;
    HBITMAP             hbm;
	HPALETTE			hpal;

    if (!hdib)
        return NULL;

    hpal = (HPALETTE)::GetStockObject(DEFAULT_PALETTE);

    lpbi = (LPBITMAPINFOHEADER)::GlobalLock(hdib);

    if (!lpbi)
        return NULL;

    hdc = ::GetDC(NULL);

    if (hpal)
	{
        hpalT = ::SelectPalette(hdc,hpal,FALSE);
        ::RealizePalette(hdc);     // GDI Bug...????
    }

	hbm = ::CreateDIBitmap(hdc,
                (LPBITMAPINFOHEADER)lpbi,
                (LONG)CBM_INIT,
                (LPSTR)lpbi + lpbi->biSize + PaletteSize(lpbi),
                (LPBITMAPINFO)lpbi,
                DIB_RGB_COLORS );

    if (hpal)
        ::SelectPalette(hdc,hpalT,FALSE);

    ::ReleaseDC(NULL,hdc);
    ::GlobalUnlock(hdib);

    return hbm;
}

// Creates a two color palette.
HPALETTE GetTwoColorPalette(COLORREF color1, COLORREF color2)
{
	NPLOGPALETTE pPal; //Local Palette
	HPALETTE pal;
	
	//Allocates space for QcQp Palette 'pPal'
	VERIFY( (UINT) (pPal = (NPLOGPALETTE)::LocalAlloc (LPTR,
		(sizeof (LOGPALETTE) +
		(sizeof (PALETTEENTRY) * 2)))));
	
	//Initialize 'pPal' fields
	pPal->palNumEntries = 2;
	pPal->palVersion = 0x300;
	
	//Inits Every color of our palette
	pPal->palPalEntry[0].peRed        =   GetRValue(color1);
	pPal->palPalEntry[0].peGreen      =   GetGValue(color1);
	pPal->palPalEntry[0].peBlue       =   GetBValue(color1);
	pPal->palPalEntry[0].peFlags      =   (BYTE) 0;
	
	pPal->palPalEntry[1].peRed        =   GetRValue(color2);
	pPal->palPalEntry[1].peGreen      =   GetGValue(color2);
	pPal->palPalEntry[1].peBlue       =   GetBValue(color2);
	pPal->palPalEntry[1].peFlags      =   (BYTE) 0;
	
	//Creates the logical palette
	pal = ::CreatePalette((LPLOGPALETTE)pPal);
	VERIFY(pal);
	
	//Free allocated memory
	VERIFY(::LocalFree((HLOCAL)pPal) == NULL);

	return pal;
}

// creates a newly colored bitmap with the first two colors replaced
// by the second two colors
HBITMAP CreateNewColoredBitmap(HBITMAP hSrc, COLORREF color1, COLORREF color2, COLORREF color3, COLORREF color4)
{
	HANDLE hDIB;
	HPALETTE hPal1, hPal2;
	HBITMAP hBitmap;
	
	// create a new palette with two colors (the old colors)
	hPal1 = GetTwoColorPalette(color1, color2);
	// create a new palette with two colors (the intended colors)
	hPal2 = GetTwoColorPalette(color3, color4);
	// create a device independent bitmap from the original using the color
	// conversion map defined by the two palettes
	hDIB = DibFromBitmap(hSrc, BI_RGB, 8, hPal1, hPal2);
	// delete the palettes
	::DeleteObject(hPal1);
	::DeleteObject(hPal2);
	// create a device dependent bitmap from the device independent bitmap
	hBitmap = BitmapFromDib(hDIB);
	// delete the device independent bitmap
	::DeleteObject(hDIB);

	return hBitmap;
}