package org.kalypso.ogc.sensor.timeseries.wq.wqtable.test;

import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.xml.sax.InputSource;

import com.sun.xml.bind.StringInputStream;

/**
 * WQTableTest
 * 
 * @author schlienger
 */
public class WQTableTest extends TestCase
{
  private WQTable wqt;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    wqt = new WQTable( new Date(), 0, new double[][]
    {
        {
            0,
            0.083 },
        {
            1,
            0.096 },
        {
            2,
            0.111 },
        {
            3,
            0.126 },
        {
            4,
            0.142 },
        {
            5,
            0.159 },
        {
            6,
            0.177 },
        {
            7,
            0.197 },
        {
            8,
            0.217 },
        {
            9,
            0.238 },
        {
            10,
            0.26 },
        {
            11,
            0.284 },
        {
            12,
            0.308 },
        {
            13,
            0.333 },
        {
            14,
            0.359 },
        {
            15,
            0.387 },
        {
            16,
            0.415 },
        {
            17,
            0.444 },
        {
            18,
            0.475 },
        {
            19,
            0.506 },
        {
            20,
            0.538 },
        {
            21,
            0.572 },
        {
            22,
            0.606 },
        {
            23,
            0.642 },
        {
            24,
            0.678 },
        {
            25,
            0.715 },
        {
            26,
            0.754 },
        {
            27,
            0.793 },
        {
            28,
            0.834 },
        {
            29,
            0.875 },
        {
            30,
            0.918 },
        {
            31,
            0.962 },
        {
            32,
            1.01 },
        {
            33,
            1.05 },
        {
            34,
            1.1 },
        {
            35,
            1.15 },
        {
            36,
            1.19 },
        {
            37,
            1.24 },
        {
            38,
            1.3 },
        {
            39,
            1.35 },
        {
            40,
            1.4 },
        {
            41,
            1.45 },
        {
            42,
            1.51 },
        {
            43,
            1.56 },
        {
            44,
            1.62 },
        {
            45,
            1.68 },
        {
            46,
            1.74 },
        {
            47,
            1.8 },
        {
            48,
            1.86 },
        {
            49,
            1.92 },
        {
            50,
            1.98 },
        {
            51,
            2.05 },
        {
            52,
            2.11 },
        {
            53,
            2.18 },
        {
            54,
            2.25 },
        {
            55,
            2.32 },
        {
            56,
            2.39 },
        {
            57,
            2.46 },
        {
            58,
            2.53 },
        {
            59,
            2.6 },
        {
            60,
            2.67 },
        {
            61,
            2.75 },
        {
            62,
            2.82 },
        {
            63,
            2.9 },
        {
            64,
            2.98 },
        {
            65,
            3.06 },
        {
            66,
            3.14 },
        {
            67,
            3.22 },
        {
            68,
            3.3 },
        {
            69,
            3.38 },
        {
            70,
            3.47 },
        {
            71,
            3.55 },
        {
            72,
            3.64 },
        {
            73,
            3.72 },
        {
            74,
            3.81 },
        {
            75,
            3.9 },
        {
            76,
            3.99 },
        {
            77,
            4.08 },
        {
            78,
            4.17 },
        {
            79,
            4.27 },
        {
            80,
            4.36 },
        {
            81,
            4.46 },
        {
            82,
            4.55 },
        {
            83,
            4.65 },
        {
            84,
            4.75 },
        {
            85,
            4.85 },
        {
            86,
            4.95 },
        {
            87,
            5.05 },
        {
            88,
            5.15 },
        {
            89,
            5.26 },
        {
            90,
            5.36 },
        {
            91,
            5.47 },
        {
            92,
            5.58 },
        {
            93,
            5.68 },
        {
            94,
            5.79 },
        {
            95,
            5.9 },
        {
            96,
            6.01 },
        {
            97,
            6.13 },
        {
            98,
            6.24 },
        {
            99,
            6.35 },
        {
            100,
            6.47 },
        {
            101,
            6.59 },
        {
            102,
            6.7 },
        {
            103,
            6.82 },
        {
            104,
            6.94 },
        {
            105,
            7.06 },
        {
            106,
            7.18 },
        {
            107,
            7.31 },
        {
            108,
            7.43 },
        {
            109,
            7.55 },
        {
            110,
            7.68 },
        {
            111,
            7.81 },
        {
            112,
            7.94 },
        {
            113,
            8.06 },
        {
            114,
            8.19 },
        {
            115,
            8.33 },
        {
            116,
            8.46 },
        {
            117,
            8.59 },
        {
            118,
            8.73 },
        {
            119,
            8.86 },
        {
            120,
            9 },
        {
            121,
            9.13 },
        {
            122,
            9.27 },
        {
            123,
            9.41 },
        {
            124,
            9.55 },
        {
            125,
            9.69 },
        {
            126,
            9.84 },
        {
            127,
            9.98 },
        {
            128,
            10.1 },
        {
            129,
            10.3 },
        {
            130,
            10.4 },
        {
            131,
            10.6 },
        {
            132,
            10.7 },
        {
            133,
            10.9 },
        {
            134,
            11 },
        {
            135,
            11.2 },
        {
            136,
            11.3 },
        {
            137,
            11.5 },
        {
            138,
            11.6 },
        {
            139,
            11.8 },
        {
            140,
            11.9 },
        {
            141,
            12.1 },
        {
            142,
            12.3 },
        {
            143,
            12.4 },
        {
            144,
            12.6 },
        {
            145,
            12.8 },
        {
            146,
            12.9 },
        {
            147,
            13.1 },
        {
            148,
            13.2 },
        {
            149,
            13.4 },
        {
            150,
            13.6 },
        {
            151,
            13.7 },
        {
            152,
            13.9 },
        {
            153,
            14.1 },
        {
            154,
            14.3 },
        {
            155,
            14.4 },
        {
            156,
            14.6 },
        {
            157,
            14.8 },
        {
            158,
            15 },
        {
            159,
            15.1 },
        {
            160,
            15.3 },
        {
            161,
            15.5 },
        {
            162,
            15.7 },
        {
            163,
            15.9 },
        {
            164,
            16 },
        {
            165,
            16.2 },
        {
            166,
            16.4 },
        {
            167,
            16.6 },
        {
            168,
            16.9 },
        {
            169,
            17 },
        {
            170,
            17.2 },
        {
            171,
            17.4 },
        {
            172,
            17.5 },
        {
            173,
            17.7 },
        {
            174,
            17.9 },
        {
            175,
            18.1 },
        {
            176,
            18.3 },
        {
            177,
            18.5 },
        {
            178,
            18.7 },
        {
            179,
            18.9 },
        {
            180,
            19.1 },
        {
            181,
            19.3 },
        {
            182,
            19.5 },
        {
            183,
            19.7 },
        {
            184,
            19.9 },
        {
            185,
            20.1 },
        {
            186,
            20.3 },
        {
            187,
            20.6 },
        {
            188,
            20.8 },
        {
            189,
            21 },
        {
            190,
            21.2 },
        {
            191,
            21.4 },
        {
            192,
            21.6 },
        {
            193,
            21.8 },
        {
            194,
            22 },
        {
            195,
            22.2 },
        {
            196,
            22.5 },
        {
            197,
            22.7 },
        {
            198,
            22.9 },
        {
            199,
            23.1 },
        {
            200,
            23.3 },
        {
            201,
            23.6 },
        {
            202,
            23.8 },
        {
            203,
            24 },
        {
            204,
            24.2 },
        {
            205,
            24.5 },
        {
            206,
            24.7 },
        {
            207,
            24.9 },
        {
            208,
            25.2 },
        {
            209,
            25.4 },
        {
            210,
            25.6 },
        {
            211,
            25.9 },
        {
            212,
            26.1 },
        {
            213,
            26.3 },
        {
            214,
            26.6 },
        {
            215,
            26.8 },
        {
            216,
            27 },
        {
            217,
            27.3 },
        {
            218,
            27.5 },
        {
            219,
            27.8 },
        {
            220,
            28 },
        {
            221,
            28.2 },
        {
            222,
            28.5 },
        {
            223,
            28.7 },
        {
            224,
            29 },
        {
            225,
            29.2 },
        {
            226,
            29.5 },
        {
            227,
            29.7 },
        {
            228,
            30 },
        {
            229,
            30.2 },
        {
            230,
            30.5 },
        {
            231,
            30.7 },
        {
            232,
            31 },
        {
            233,
            31.3 },
        {
            234,
            31.5 },
        {
            235,
            31.8 },
        {
            236,
            32 },
        {
            237,
            32.3 },
        {
            238,
            32.6 },
        {
            239,
            32.8 },
        {
            240,
            33.1 },
        {
            241,
            33.3 },
        {
            242,
            33.6 },
        {
            243,
            33.9 },
        {
            244,
            34.2 },
        {
            245,
            34.4 },
        {
            246,
            34.7 },
        {
            247,
            35 },
        {
            248,
            35.2 },
        {
            249,
            35.5 },
        {
            250,
            35.8 },
        {
            251,
            36.1 },
        {
            252,
            36.3 },
        {
            253,
            36.6 },
        {
            254,
            36.9 },
        {
            255,
            37.2 },
        {
            256,
            37.5 },
        {
            257,
            37.7 },
        {
            258,
            38 },
        {
            259,
            38.3 },
        {
            260,
            38.6 },
        {
            261,
            38.9 },
        {
            262,
            39.2 },
        {
            263,
            39.5 },
        {
            264,
            39.7 },
        {
            265,
            40 },
        {
            266,
            40.3 },
        {
            267,
            40.6 },
        {
            268,
            40.9 },
        {
            269,
            41.2 },
        {
            270,
            41.5 },
        {
            271,
            41.8 },
        {
            272,
            42.1 },
        {
            273,
            42.4 },
        {
            274,
            42.7 },
        {
            275,
            43 },
        {
            276,
            43.3 },
        {
            277,
            43.6 },
        {
            278,
            43.9 },
        {
            279,
            44.2 },
        {
            280,
            44.5 },
        {
            281,
            44.8 },
        {
            282,
            45.2 },
        {
            283,
            45.5 },
        {
            284,
            45.8 },
        {
            285,
            46.1 },
        {
            286,
            46.4 },
        {
            287,
            46.7 },
        {
            288,
            47 },
        {
            289,
            47.4 },
        {
            290,
            47.7 },
        {
            291,
            48 },
        {
            292,
            48.3 },
        {
            293,
            48.6 },
        {
            294,
            49 },
        {
            295,
            49.3 },
        {
            296,
            49.6 },
        {
            297,
            49.9 },
        {
            298,
            50.3 },
        {
            299,
            50.6 },
        {
            300,
            50.9 } } );
  }

  public void testGet1() throws WQException
  {
    double Q = 21.8;

    double W = wqt.getWFor( Q );

    System.out.println( "For Q= " + Q + " W= " + W ); //$NON-NLS-1$ //$NON-NLS-2$

    System.out.println( "For W= " + W + " Q= " + wqt.getQFor( W ) ); //$NON-NLS-1$ //$NON-NLS-2$

    assertTrue( Double.compare( Q, wqt.getQFor( W ) ) == 0 );
  }

  public void testGet2() throws WQException
  {
    double Q = 26.5;

    double W = wqt.getWFor( Q );

    System.out.println( "For Q= " + Q + " W= " + W ); //$NON-NLS-1$ //$NON-NLS-2$

    System.out.println( "For W= " + W + " Q= " + wqt.getQFor( W ) ); //$NON-NLS-1$ //$NON-NLS-2$

    assertTrue( Double.compare( Q, wqt.getQFor( W ) ) == 0 );
  }

  public void testGet3()
  {
    double Q = 0.075;

    try
    {
      wqt.getWFor( Q );

      assertNull( "Should not come here" ); //$NON-NLS-1$
    }
    catch( WQException e )
    {
      // ignore
    }
  }

  public void testGet4()
  {
    double Q = 51;

    try
    {
      wqt.getWFor( Q );

      assertNull( "Should not come here" ); //$NON-NLS-1$
    }
    catch( WQException e )
    {
      // ignore
    }
  }

  public void testDiverses() throws WQException, ParseException
  {
    final InputStream stream = getClass().getResourceAsStream( "ratingtable-example.xml" ); //$NON-NLS-1$

    final WQTableSet tableSet = WQTableFactory.parse( new InputSource( stream ) );

    System.out.println( tableSet );

    final SimpleDateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ); //$NON-NLS-1$

    final Date d1 = sdf.parse( "01.04.1996 04:32" ); //$NON-NLS-1$
    final WQTable table1 = tableSet.getFor( d1 );
    // TODO: why 01:00, the file has no date??
//    final Date td1 = sdf.parse( "01.11.1997 01:00" );
    final Date td1 = sdf.parse( "01.11.1997 00:00" ); //$NON-NLS-1$
    assertTrue( table1.getValidity().equals( td1 ) );

    final Date d2 = sdf.parse( "04.11.1998 3:55" ); //$NON-NLS-1$
    final WQTable table2 = tableSet.getFor( d2 );
    final Date td2 = sdf.parse( "01.11.1998 00:00" ); //$NON-NLS-1$
//    final Date td2 = sdf.parse( "01.11.1998 01:00" );
    assertTrue( table2.getValidity().equals( td2 ) );

    final Date d3 = sdf.parse( "01.04.2000 6:34" ); //$NON-NLS-1$
    final WQTable table3 = tableSet.getFor( d3 );
    final Date td3 = sdf.parse( "01.10.1999 00:00" ); //$NON-NLS-1$
//    final Date td3 = sdf.parse( "01.10.1999 02:00" );
    assertTrue( table3.getValidity().equals( td3 ) );

    final String xml = WQTableFactory.createXMLString( tableSet );
    final WQTableSet tmp = WQTableFactory.parse( new InputSource( new StringInputStream( xml ) ) );
    final String xmlAgain = WQTableFactory.createXMLString( tmp );
    System.out.println( xml );
    System.out.println( xmlAgain );
  }
}
