package view;

/**
 * This class contains several static methods that create on-the-fly look-up
 * tables.
 */
public class LUTs
{

  /** The red lut */
  public final static short[][] red()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = 0;
      lut[i][2] = 0;
    }
    return lut;
  }

  /** The green lut */
  public final static short[][] green()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = 0;
      lut[i][1] = i;
      lut[i][2] = 0;
    }
    return lut;
  }

  /** The blue lut */
  public final static short[][] blue()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = 0;
      lut[i][1] = 0;
      lut[i][2] = i;
    }
    return lut;
  }

  /** The cyan lut */
  public final static short[][] cyan()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = 0;
      lut[i][1] = i;
      lut[i][2] = i;
    }
    return lut;
  }

  /** The magenta lut */
  public final static short[][] magenta()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = 0;
      lut[i][2] = i;
    }
    return lut;
  }

  /** The yellow lut */
  public final static short[][] yellow()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = i;
      lut[i][2] = 0;
    }
    return lut;
  }

  /** The gray lut */
  public final static short[][] gray()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = i;
      lut[i][2] = i;
    }
    return lut;
  }

  /** The inverted gray lut */
  public final static short[][] inverted_gray()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 255 - i );
      lut[i][1] = (short)( 255 - i );
      lut[i][2] = (short)( 255 - i );
    }
    return lut;
  }

  /** The two-levels gray lut */
  public final static short[][] two_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 128 * (short)( i / 128 ) );
      lut[i][1] = (short)( 128 * (short)( i / 128 ) );
      lut[i][2] = (short)( 128 * (short)( i / 128 ) );
    }
    return lut;
  }

  /** The four-levels gray lut */
  public final static short[][] four_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 64 * (short)( i / 64 ) );
      lut[i][1] = (short)( 64 * (short)( i / 64 ) );
      lut[i][2] = (short)( 64 * (short)( i / 64 ) );
    }
    return lut;
  }

  /** The eight-levels gray lut */
  public final static short[][] eight_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 32 * (short)( i / 32 ) );
      lut[i][1] = (short)( 32 * (short)( i / 32 ) );
      lut[i][2] = (short)( 32 * (short)( i / 32 ) );
    }
    return lut;
  }

  /** The sixteen-levels gray lut */
  public final static short[][] sixteen_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 16 * (short)( i / 16 ) );
      lut[i][1] = (short)( 16 * (short)( i / 16 ) );
      lut[i][2] = (short)( 16 * (short)( i / 16 ) );
    }
    return lut;
  }

  /** The thyrty-two-levels gray lut */
  public final static short[][] thirty_two_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 8 * (short)( i / 8 ) );
      lut[i][1] = (short)( 8 * (short)( i / 8 ) );
      lut[i][2] = (short)( 8 * (short)( i / 8 ) );
    }
    return lut;
  }

  /** The sixty-four-levels gray lut */
  public final static short[][] sixty_four_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 4 * (short)( i / 4 ) );
      lut[i][1] = (short)( 4 * (short)( i / 4 ) );
      lut[i][2] = (short)( 4 * (short)( i / 4 ) );
    }
    return lut;
  }

  /** The hundred-twenty-eight-levels gray lut */
  public final static short[][] hundred_twenty_eight_levels()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 2 * (short)( i / 2 ) );
      lut[i][1] = (short)( 2 * (short)( i / 2 ) );
      lut[i][2] = (short)( 2 * (short)( i / 2 ) );
    }
    return lut;
  }

  /** The red-cyan lut */
  public final static short[][] red_cyan()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 255 - i );
      lut[i][1] = i;
      lut[i][2] = i;
    }
    return lut;
  }

  /** The green-magenta lut */
  public final static short[][] green_magenta()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = (short)( 255 - i );
      lut[i][2] = i;
    }
    return lut;
  }

  /** The blue-yellow lut */
  public final static short[][] blue_yellow()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = i;
      lut[i][2] = (short)( 255 - i );
    }
    return lut;
  }

  /** The sin lut (rgb order) */
  public final static short[][] sin_rgb()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (rbg order) */
  public final static short[][] sin_rbg()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (grb order) */
  public final static short[][] sin_grb()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (gbr order) */
  public final static short[][] sin_gbr()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (brg order) */
  public final static short[][] sin_brg()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (bgr order) */
  public final static short[][] sin_bgr()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][2] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i - 127 ) / 255 ) ) );
      lut[i][1] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i ) / 255 ) ) );
      lut[i][0] = (short)( 127 * ( 1 + Math.sin( Math.PI * ( i + 127 ) / 255 ) ) );
    }
    return lut;
  }

  /** The sin lut (rgb order, black as zero) */
  public final static short[][] sin_rgb_0()
  {
    short[][] lut = sin_rgb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sin lut (rbg order, black as zero) */
  public final static short[][] sin_rbg_0()
  {
    short[][] lut = sin_rbg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sin lut (grb order, black as zero) */
  public final static short[][] sin_grb_0()
  {
    short[][] lut = sin_grb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sin lut (gbr order, black as zero) */
  public final static short[][] sin_gbr_0()
  {
    short[][] lut = sin_gbr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sin lut (brg order, black as zero) */
  public final static short[][] sin_brg_0()
  {
    short[][] lut = sin_brg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sin lut (bgr order, black as zero) */
  public final static short[][] sin_bgr_0()
  {
    short[][] lut = sin_bgr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (rgb order) */
  public final static short[][] sqrt_rgb()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][1] = i;
      lut[i][2] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (rbg order) */
  public final static short[][] sqrt_rbg()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][2] = i;
      lut[i][1] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (grb order) */
  public final static short[][] sqrt_grb()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][1] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][0] = i;
      lut[i][2] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (gbr order) */
  public final static short[][] sqrt_gbr()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][1] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][2] = i;
      lut[i][0] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (brg order) */
  public final static short[][] sqrt_brg()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][2] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][1] = i;
      lut[i][0] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (bgr order) */
  public final static short[][] sqrt_bgr()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( 255 - 15 * Math.sqrt( i ) );
      lut[i][1] = i;
      lut[i][2] = (short)( 255 - 15 * Math.sqrt( 255 - i ) );
    }
    return lut;
  }

  /** The sqrt lut (rgb order, black as zero) */
  public final static short[][] sqrt_rgb_0()
  {
    short[][] lut = sqrt_rgb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (rbg order, black as zero) */
  public final static short[][] sqrt_rbg_0()
  {
    short[][] lut = sqrt_rbg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (brg order, black as zero) */
  public final static short[][] sqrt_brg_0()
  {
    short[][] lut = sqrt_brg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (bgr order, black as zero) */
  public final static short[][] sqrt_bgr_0()
  {
    short[][] lut = sqrt_bgr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (gbr order, black as zero) */
  public final static short[][] sqrt_gbr_0()
  {
    short[][] lut = sqrt_gbr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The sqrt lut (grb order, black as zero) */
  public final static short[][] sqrt_grb_0()
  {
    short[][] lut = sqrt_grb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (rgb order) */
  public final static short[][] hue_rgb()
  {
    short[][] lut = new short[256][3];
    float r = 255, g = 0, b = 0;
    for( short i = 0; i < 43; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      g += 5.9;
    }
    r = 255;
    g = 255;
    b = 0;
    for( short i = 43; i < 86; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      r -= 5.9;
    }
    r = 0;
    g = 255;
    b = 0;
    for( short i = 86; i < 128; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      b += 5.9;
    }
    r = 0;
    g = 255;
    b = 255;
    for( short i = 128; i < 171; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      g -= 5.9;
    }
    r = 0;
    g = 0;
    b = 255;
    for( short i = 171; i < 213; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      r += 5.9;
    }
    r = 255;
    g = 0;
    b = 255;
    for( short i = 213; i < 256; i++ )
    {
      lut[i][0] = (short)r;
      lut[i][1] = (short)g;
      lut[i][2] = (short)b;
      b -= 5.9;
    }
    return lut;
  }

  /** The hue lut (rbg order) */
  public final static short[][] hue_rbg()
  {
    short[][] temp = hue_rgb();
    for( int i = 0; i < 256; i++ )
    {
      short t = temp[i][1];
      temp[i][1] = temp[i][2];
      temp[i][2] = t;
    }
    return temp;
  }

  /** The hue lut (grb order) */
  public final static short[][] hue_grb()
  {
    short[][] temp = hue_rgb();
    for( int i = 0; i < 256; i++ )
    {
      short t = temp[i][0];
      temp[i][0] = temp[i][1];
      temp[i][1] = t;
    }
    return temp;
  }

  /** The hue lut (gbr order) */
  public final static short[][] hue_gbr()
  {
    short[][] temp = hue_rgb();
    for( int i = 0; i < 256; i++ )
    {
      short r = temp[i][0];
      short g = temp[i][1];
      short b = temp[i][2];
      temp[i][0] = b;
      temp[i][1] = r;
      temp[i][2] = g;
    }
    return temp;
  }

  /** The hue lut (brg order) */
  public final static short[][] hue_brg()
  {
    short[][] temp = hue_rgb();
    for( int i = 0; i < 256; i++ )
    {
      short r = temp[i][0];
      short g = temp[i][1];
      short b = temp[i][2];
      temp[i][0] = g;
      temp[i][1] = b;
      temp[i][2] = r;
    }
    return temp;
  }

  /** The hue lut (bgr order) */
  public final static short[][] hue_bgr()
  {
    short[][] temp = hue_rgb();
    for( int i = 0; i < 256; i++ )
    {
      short t = temp[i][0];
      temp[i][0] = temp[i][2];
      temp[i][2] = t;
    }
    return temp;
  }

  /** The hue lut (rgb order, black as zero) */
  public final static short[][] hue_rgb_0()
  {
    short[][] lut = hue_rgb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (rbg order, black as zero) */
  public final static short[][] hue_rbg_0()
  {
    short[][] lut = hue_rbg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (grb order, black as zero) */
  public final static short[][] hue_grb_0()
  {
    short[][] lut = hue_grb();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (gbr order, black as zero) */
  public final static short[][] hue_gbr_0()
  {
    short[][] lut = hue_gbr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (brg order, black as zero) */
  public final static short[][] hue_brg_0()
  {
    short[][] lut = hue_brg();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The hue lut (bgr order, black as zero) */
  public final static short[][] hue_bgr_0()
  {
    short[][] lut = hue_bgr();
    lut[0][0] = lut[0][1] = lut[0][2] = 0;
    return lut;
  }

  /** The red saw lut (number of teeth is passed as an argument) */
  private final static short[][] red_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short r = 255;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = r;
      lut[i][1] = i;
      lut[i][2] = i;
      r -= teeth;
      if( r < 0 )
        r = 255;
    }
    return lut;
  }

  /** The green saw lut (number of teeth is passed as an argument) */
  private final static short[][] green_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short r = 255;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = r;
      lut[i][2] = i;
      r -= teeth;
      if( r < 0 )
        r = 255;
    }
    return lut;
  }

  /** The blue saw lut (number of teeth is passed as an argument) */
  private final static short[][] blue_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short r = 255;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = i;
      lut[i][2] = r;
      r -= teeth;
      if( r < 0 )
        r = 255;
    }
    return lut;
  }

  /** The red saw lut (2 teeth) */
  public final static short[][] red_saw_2()
  {
    return red_saw( (short)2 );
  }

  /** The red saw lut (4 teeth) */
  public final static short[][] red_saw_4()
  {
    return red_saw( (short)4 );
  }

  /** The red saw lut (8 teeth) */
  public final static short[][] red_saw_8()
  {
    return red_saw( (short)8 );
  }

  /** The green saw lut (2 teeth) */
  public final static short[][] green_saw_2()
  {
    return green_saw( (short)2 );
  }

  /** The green saw lut (4 teeth) */
  public final static short[][] green_saw_4()
  {
    return green_saw( (short)4 );
  }

  /** The green saw lut (8 teeth) */
  public final static short[][] green_saw_8()
  {
    return green_saw( (short)8 );
  }

  /** The blue saw lut (2 teeth) */
  public final static short[][] blue_saw_2()
  {
    return blue_saw( (short)2 );
  }

  /** The blue saw lut (4 teeth) */
  public final static short[][] blue_saw_4()
  {
    return blue_saw( (short)4 );
  }

  /** The blue saw lut (8 teeth) */
  public final static short[][] blue_saw_8()
  {
    return blue_saw( (short)8 );
  }

  /** The red-green saw lut (number of teeth passed as argument) */
  private final static short[][] red_green_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short r = 255;
    short g = 0;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = r;
      lut[i][1] = g;
      lut[i][2] = i;
      r -= teeth;
      if( r < 0 )
        r = 255;
      g += teeth;
      if( g > 255 )
        g = 0;
    }
    return lut;
  }

  /** The red_green saw lut (2 teeth) */
  public final static short[][] red_green_saw_2()
  {
    return red_green_saw( (short)2 );
  }

  /** The red_green saw lut (4 teeth) */
  public final static short[][] red_green_saw_4()
  {
    return red_green_saw( (short)4 );
  }

  /** The red_green saw lut (8 teeth) */
  public final static short[][] red_green_saw_8()
  {
    return red_green_saw( (short)8 );
  }

  /** The red-blue saw lut (number of teeth passed as argument) */
  private final static short[][] red_blue_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short r = 255;
    short b = 0;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = r;
      lut[i][1] = i;
      lut[i][2] = b;
      r -= teeth;
      if( r < 0 )
        r = 255;
      b += teeth;
      if( b > 255 )
        b = 0;
    }
    return lut;
  }

  /** The red_blue saw lut (2 teeth) */
  public final static short[][] red_blue_saw_2()
  {
    return red_blue_saw( (short)2 );
  }

  /** The red_blue saw lut (4 teeth) */
  public final static short[][] red_blue_saw_4()
  {
    return red_blue_saw( (short)4 );
  }

  /** The red_blue saw lut (8 teeth) */
  public final static short[][] red_blue_saw_8()
  {
    return red_blue_saw( (short)8 );
  }

  /** The red-blue saw lut (number of teeth passed as argument) */
  private final static short[][] green_blue_saw( short teeth )
  {
    short[][] lut = new short[256][3];
    short g = 255;
    short b = 0;
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = i;
      lut[i][1] = g;
      lut[i][2] = b;
      g -= teeth;
      if( g < 0 )
        g = 255;
      b += teeth;
      if( b > 255 )
        b = 0;
    }
    return lut;
  }

  /** The green_blue saw lut (2 teeth) */
  public final static short[][] green_blue_saw_2()
  {
    return green_blue_saw( (short)2 );
  }

  /** The green_blue saw lut (4 teeth) */
  public final static short[][] green_blue_saw_4()
  {
    return green_blue_saw( (short)4 );
  }

  /** The green_blue saw lut (8 teeth) */
  public final static short[][] green_blue_saw_8()
  {
    return green_blue_saw( (short)8 );
  }

  /** The random_256 lut (values are totally independent) */
  public final static short[][] random_256()
  {
    short[][] lut = new short[256][3];
    for( short i = 0; i < 256; i++ )
    {
      lut[i][0] = (short)( Math.random() * 256 );
      lut[i][1] = (short)( Math.random() * 256 );
      lut[i][2] = (short)( Math.random() * 256 );
    }
    return lut;
  }

  /** The random_32 lut (values are totally independent) */
  public final static short[][] random_32()
  {
    short[][] lut = new short[256][3];
    short r = 0, g = 0, b = 0;
    for( short i = 0; i < 256; i++ )
    {
      if( i % 8 == 0 )
      {
        r = (short)( Math.random() * 256 );
        g = (short)( Math.random() * 256 );
        b = (short)( Math.random() * 256 );
      }
      lut[i][0] = r;
      lut[i][1] = g;
      lut[i][2] = b;
    }
    return lut;
  }

  /** The random_8 lut (values are totally independent) */
  public final static short[][] random_8()
  {
    short[][] lut = new short[256][3];
    short r = 0, g = 0, b = 0;
    for( short i = 0; i < 256; i++ )
    {
      if( i % 32 == 0 )
      {
        r = (short)( Math.random() * 256 );
        g = (short)( Math.random() * 256 );
        b = (short)( Math.random() * 256 );
      }
      lut[i][0] = r;
      lut[i][1] = g;
      lut[i][2] = b;
    }
    return lut;
  }

}