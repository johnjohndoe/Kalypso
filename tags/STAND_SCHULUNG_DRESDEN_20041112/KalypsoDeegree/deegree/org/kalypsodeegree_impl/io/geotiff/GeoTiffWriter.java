// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/io/geotiff/GeoTiffWriter.java,v 1.8
// 2004/08/18 08:50:17 axel_schaefer Exp $
/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.
 
 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.
 
 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
 Contact:
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.io.geotiff;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.apache.batik.ext.awt.image.codec.tiff.TIFFEncodeParam;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFField;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFImageEncoder;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.cs.CS_GeocentricCoordinateSystem;
import org.opengis.cs.CS_GeographicCoordinateSystem;
import org.opengis.cs.CS_ProjectedCoordinateSystem;

/**
 * This class is for writing GeoTIFF files from any java.awt.image. At that
 * time, only writing the Bounding Box is available.
 * 
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </A>
 * @author last edited by: $Author$
 * @version 2.0. $Revision$, $Date$
 * @since 2.0
 */
public class GeoTiffWriter
{

  private ArrayList tiffields = null;

  private HashMap geoKeyDirectoryTag = null;

  private BufferedImage bi = null;

  /*
   * private constructor. called from any other public constructor
   */
  private GeoTiffWriter( GM_Envelope envelope, double resx, double resy, CS_CoordinateSystem crs )
      throws GeoTiffException
  {
    this.tiffields = new ArrayList();
    this.geoKeyDirectoryTag = new HashMap();
    int[] header =
    { 1, 2, 0 };
    // sets the header. this key must be overwritten in the write-method.
    addKeyToGeoKeyDirectoryTag( 1, header );
    // sets the boundingbox (with envelope and resolution)
    setBoxInGeoTIFF( envelope, resx, resy );
    // sets the CoordinateSystem
    setCoordinateSystem( crs );
  }

  /**
   * creates an GeoTiffWriter instance from an java.awt.image.
   * 
   * 
   * @param image
   *          the image, to be transformed to a GeoTIFF.
   * @param envelope
   *          the BoundingBox, the GeoTIFF should have
   * @param resx
   *          The X-Resolution
   * @param resy
   *          The Y-Resolution
   */
  public GeoTiffWriter( BufferedImage image, GM_Envelope envelope, double resx, double resy,
      CS_CoordinateSystem crs ) throws GeoTiffException
  {
    this( envelope, resx, resy, crs );
    this.bi = image;
  }

  /**
   * creates an GeoTiffWriter instance.
   * 
   * @param img
   * @param envelope
   * @param resx
   * @param resy
   */
  public GeoTiffWriter( byte[][] img, GM_Envelope envelope, double resx, double resy,
      CS_CoordinateSystem crs ) throws GeoTiffException
  {
    // TODO constructor byte[][]
    this( envelope, resx, resy, crs );
  }

  /**
   * creates an GeoTiffWriter instance.
   * 
   * @param img
   * @param envelope
   * @param resx
   * @param resy
   */
  public GeoTiffWriter( short[][] img, GM_Envelope envelope, double resx, double resy,
      CS_CoordinateSystem crs ) throws GeoTiffException
  {
    this( envelope, resx, resy, crs );
  }

  /**
   * creates an GeoTiffWriter instance.
   * 
   * @param img
   * @param envelope
   * @param resx
   * @param resy
   */
  public GeoTiffWriter( float[][] img, GM_Envelope envelope, double resx, double resy,
      CS_CoordinateSystem crs ) throws GeoTiffException
  {
    // TODO constructor float[][]
    this( envelope, resx, resy, crs );

  }

  /**
   * returns the GeoKeys as an array of Tiff Fields.
   * 
   * @return an array of TIFFFields
   */
  private TIFFField[] getGeoTags()
  {
    TIFFField[] extraFields = null;

    if( this.tiffields != null && this.tiffields.size() > 0 )
    {
      extraFields = new TIFFField[this.tiffields.size()];
      for( int i = 0; i < extraFields.length; i++ )
      {
        extraFields[i] = (TIFFField)this.tiffields.get( i );
      }
    }
    return extraFields;
  }

  /**
   * gets the GeoKeyDirectoryTag as a chararrary.
   * 
   * @return
   */
  private char[] getGeoKeyDirectoryTag()
  {
    char[] ch = null;

    // check, if it contains more fields than the header
    if( this.geoKeyDirectoryTag.size() > 1 )
    {
      ch = new char[this.geoKeyDirectoryTag.size() * 4];
      Set set = this.geoKeyDirectoryTag.keySet();
      Object[] o = set.toArray();

      Integer keyID, tiffTagLocation, count, valueOffset = null;
      int[] temparray = new int[3];
      char c = 0;

      // o.length is equals this.geoKeyDirectoryTag.size()
      for( int i = 0; i < o.length; i++ )
      {
        // get the key-ID from the ObjectArray 'o'
        keyID = (Integer)o[i];
        // get the values of the HashMap (int[]) at the key keyID
        temparray = (int[])this.geoKeyDirectoryTag.get( keyID );
        ch[i * 4] = (char)keyID.intValue();
        ch[i * 4 + 1] = (char)temparray[0];
        ch[i * 4 + 2] = (char)temparray[1];
        ch[i * 4 + 3] = (char)temparray[2];
      }
    }

    // DEBUG
    //        for (int i = 0; i < ch.length; i = i + 4) {
    //            System.out.println("[" + i + "].KEY: " + (int) ch[i] + " \t"
    //                    + (int) ch[i + 1] + "\t" + (int) ch[i + 2] + "\t"
    //                    + (int) ch[i + 3]);
    //        }

    return ch;
  }

  /**
   * 
   * @param key
   * @param values
   */
  private void addKeyToGeoKeyDirectoryTag( int key, int[] values )
  {
    this.geoKeyDirectoryTag.put( new Integer( key ), values );
  }

  /**
   * 
   * @param string
   */
  private void addString2GeoAsciiParamsTag( String string )
  {
    // TODO GeoAsciiParamsTag
    System.out.println( "String: " + string );

    this.tiffields.contains( new Integer( GeoTiffTag.GeoAsciiParamsTag ) );

    // TIFFField field =
    // this.tiffields.getField(GeoTiffTag.GeoAsciiParamsTag);

    //        String gapt = field.getAsString(0);
    //        System.out.println(gapt);
    //        StringTokenizer st = new StringTokenizer(gapt, "|");
    //        System.out.println(st.countTokens());
    //        String[] gapt_fields = new String[st.countTokens()];
    //        int i = 0;
    //        while (st.hasMoreTokens()) {
    //            gapt_fields[i++] = st.nextToken();
    //        }
    //        for (int j = 0; j < gapt_fields.length; j++) {
    //            System.out.println(gapt_fields[j]);
    //        }

  }

  /**
   * Writes the GeoTIFF as a BufferedImage to an OutputStream. The OutputStream
   * isn't closed after the method.
   * 
   * @param os
   *          the output stream, which has to be written.
   * @throws IOException
   */
  public void write( OutputStream os ) throws IOException
  {

    // read out GeoKeyDirectoryTag
    if( this.geoKeyDirectoryTag.size() > 1 )
    {
      // overwriter header with *real* size of GeoKeyDirectoryTag
      int[] header =
      { 1, 2, this.geoKeyDirectoryTag.size() - 1 };
      addKeyToGeoKeyDirectoryTag( 1, header );

      char[] ch = getGeoKeyDirectoryTag();

      // int tag, int type, int count, java.lang.Object data
      TIFFField geokeydirectorytag = new TIFFField( GeoTiffTag.GeoKeyDirectoryTag,
          TIFFField.TIFF_SHORT, ch.length, ch );
      this.tiffields.add( geokeydirectorytag );
    }

    // get the geokeys
    TIFFField[] tiffields_array = getGeoTags();

    TIFFEncodeParam encodeParam = new TIFFEncodeParam();
    if( tiffields_array != null && tiffields_array.length > 0 )
    {
      encodeParam.setExtraFields( tiffields_array );
    }
    TIFFImageEncoder encoder = new TIFFImageEncoder( os, encodeParam );

    // void encoder( java.awt.image.RenderedImage im )
    encoder.encode( bi );
  }

  // ************************************************************************
  // BoundingBox
  // ************************************************************************
  /**
   * description: Extracts the GeoKeys of the GeoTIFF. The Following Tags will
   * be extracted(http://www.remotesensing.org/geotiff/spec/geotiffhome.html):
   * <ul>
   * <li>ModelPixelScaleTag = 33550 (SoftDesk)
   * <li>ModelTiepointTag = 33922 (Intergraph)
   * </ul>
   * implementation status: working
   */
  private void setBoxInGeoTIFF( GM_Envelope envelope, double resx, double resy )
      throws GeoTiffException
  {

    double[] resolution =
    { resx, resy, 0.0 };
    // ModelPixelScaleTag:
    // Tag = 33550
    // Type = DOUBLE (IEEE Double precision)
    // N = 3
    // Owner: SoftDesk
    TIFFField modelPixelScaleTag = new TIFFField( GeoTiffTag.ModelPixelScaleTag,
        TIFFField.TIFF_DOUBLE, 3, resolution );

    this.tiffields.add( modelPixelScaleTag );

    // ModelTiepointTag:
    // calculate the first points for the upper-left corner {0,0,0} of the
    // tiff
    double tp_01x = 0.0; // (0, val1)
    double tp_01y = 0.0; // (1, val2)
    double tp_01z = 0.0; // (2) z-value. not needed

    // the real-world coordinates for the upper points (tp_01.)
    // these are the unknown variables which have to be calculated.
    double tp_02x = 0.0; // (3, val4)
    double tp_02y = 0.0; // (4, val5)
    double tp_02z = 0.0; // (5) z-value. not needed

    double xmin = envelope.getMin().getX();
    double ymax = envelope.getMax().getY();

    // transform this equation: xmin = ?[val4] - ( tp_01x * resx )
    tp_02x = xmin + ( tp_01x * resx );

    // transform this equation: ymax = ?[val5] + ( tp_01y * resy )
    tp_02y = ymax + ( tp_01y * resy );

    double[] tiepoint =
    { tp_01x, tp_01y, tp_01z, tp_02x, tp_02y, tp_02z };

    // ModelTiepointTag:
    // Tag = 33922 (8482.H)
    // Type = DOUBLE (IEEE Double precision)
    // N = 6*K, K = number of tiepoints
    // Alias: GeoreferenceTag
    // Owner: Intergraph
    TIFFField modelTiepointTag = new TIFFField( GeoTiffTag.ModelTiepointTag, TIFFField.TIFF_DOUBLE,
        6, tiepoint );

    this.tiffields.add( modelTiepointTag );
  }

  // ************************************************************************
  // CoordinateSystem
  // ************************************************************************
  /**
   *  
   */
  private void setCoordinateSystem( CS_CoordinateSystem crs ) throws GeoTiffException
  {

    if( crs instanceof CS_GeographicCoordinateSystem )
    {
      System.out.println( "GeographicCoordinateSystem" );
      setCS_GeographicCoordinateSystem( (CS_GeographicCoordinateSystem)crs );

    }
    else if( crs instanceof CS_GeocentricCoordinateSystem )
    {
      System.out.println( "GeocentricCoordinateSystem" );
      // TODO CS_GeocentricCoordinateSystem

    }
    else if( crs instanceof CS_ProjectedCoordinateSystem )
    {
      System.out.println( "ProjectedCoordinateSystem" );
      setCS_ProjectedCoordinateSystem( (CS_ProjectedCoordinateSystem)crs );

    }
    else
    {
      // user-defined, unknown or broken.
      // System.out.println("unknown Coorinate System");

      // CS_CompoundCoordinateSystem, CS_FittedCoordinateSystem,
      // CS_HorizontalCoordinateSystem, CS_LocalCoordinateSystem,
      // CS_VerticalCoordinateSystem

      // CS_GeocentricCoordinateSystem, CS_GeographicCoordinateSystem,
      // CS_ProjectedCoordinateSystem
    }
  }

  /**
   * 
   * @param crs
   * @throws GeoTiffException
   */
  private void setCS_GeographicCoordinateSystem( CS_GeographicCoordinateSystem crs )
      throws GeoTiffException
  {
    // add GTModelTypeGeoKey
    int[] values_GTModelTypeGeoKey =
    { 0, 1, 2 };
    addKeyToGeoKeyDirectoryTag( GeoTiffKey.GTModelTypeGeoKey, values_GTModelTypeGeoKey );

    try
    {
      String horizontalDatum = crs.getHorizontalDatum().getName();
      if( Geographic_CS_Codes.contains_Geodectic_Datum_Code( horizontalDatum ) )
      {
        int[] geographictypegeokey =
        { 0, 1, Geographic_CS_Codes.getGeogrpahicCSTypeCode( horizontalDatum ) };
        addKeyToGeoKeyDirectoryTag( GeoTiffKey.GeographicTypeGeoKey, geographictypegeokey );
      }
      else
      {
        throw new GeoTiffException( "Error in determining Horizontal Datum Name:\n" + " "
            + horizontalDatum + " ist not registered in Geodetic Datum Codes." );
      }

    }
    catch( RemoteException e )
    {
      throw new GeoTiffException( "RemoteException: " + e.getMessage() );
    }
  }

  /**
   * 
   * @param crs
   * @throws GeoTiffException
   */
  private void setCS_ProjectedCoordinateSystem( CS_ProjectedCoordinateSystem crs )
      throws GeoTiffException
  {
    System.out.println( crs );

    // add GTModelTypeGeoKey
    int[] values_GTModelTypeGeoKey =
    { 0, 1, 1 };
    addKeyToGeoKeyDirectoryTag( GeoTiffKey.GTModelTypeGeoKey, values_GTModelTypeGeoKey );

    try
    {
      String name = crs.getName();
      int epsg_key = 0;

      if( name.startsWith( "EPSG:" ) )
      {
        // Substring: "EPSG:"
        name = name.substring( 5 );
        epsg_key = Integer.parseInt( name );
      }
      else
      {
        throw new GeoTiffException( "Error: CS_Projected Coordinate System '" + name
            + "' not implemented." );
      }

      if( Projected_CS_Codes.containsProjectedCSTypeCode( epsg_key ) )
      {
        int[] values_ProjectedCSTypeGeoKey =
        { 0, 1, epsg_key };
        addKeyToGeoKeyDirectoryTag( GeoTiffKey.ProjectedCSTypeGeoKey, values_ProjectedCSTypeGeoKey );
        addString2GeoAsciiParamsTag( Projected_CS_Codes.getProjectedCSTypeCode( epsg_key ) );
      }
      else
      {
        // TODO what if false?
        throw new GeoTiffException( "Error: CS_Projected Coordinate System '" + epsg_key
            + "' not implemented." );
      }

    }
    catch( RemoteException e )
    {
      throw new GeoTiffException( "RemoteException: " + e.getMessage() );
    }

  }

  // ************************************************************************
  // ************************************************************************
  // ************************************************************************

  /**
   * main method for testing.
   * 
   * @param args
   */
  public static void main( String[] args )
  {

    File file = new File( "d:/geotiff/intergraph/gauss_k.jpg" );
    File f = new File( "d:/geotiff/intergraph/output.tif" );
    System.out.println( "reading: " + file.getAbsolutePath() + ", writing: " + f.getAbsolutePath() );

    // creating GM_Envelope envelope
    // -122.6261000000582; 37.453100000000404; -122.07770000005861; 38.0;
    // -19.99583244323731; 39.99583174288593; 20.004169642922694;
    // 89.99583435058594;
    double minx = 613855.956;
    double miny = 217325.67999999618;
    double maxx = 624010.153999998;
    double maxy = 227479.878;
    GM_Envelope envelope = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    // EPSG:28405
    CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( "EPSG:28405" );
    CS_CoordinateSystem crs = Adapters.getDefault().export( cs );

    // creating resolution resx and resy
    // 0.000299999999999778;0.000299999999999778;
    double resx = 0.0333333350718;
    double resy = 0.0333333350718;

    FileOutputStream fos = null;
    try
    {
      BufferedImage bi = javax.imageio.ImageIO.read( file );
      fos = new FileOutputStream( f );
      GeoTiffWriter gtw = new GeoTiffWriter( bi, envelope, resx, resy, crs );
      gtw.write( fos );
      fos.close();
    }
    catch( GeoTiffException e )
    {
      e.printStackTrace();
    }
    catch( FileNotFoundException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }

    System.out.println( "TIFF '" + f.getAbsolutePath() + "' written. Finished." );
  }

}

/*
 * ****************************************************************************
 * Changes to this class. What the people have been up to:
 * 
 * $Log$
 * Revision 1.1  2004/10/07 14:09:20  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.1 2004/08/31 12:41:08 doemming ***
 * empty log message *** Revision 1.8 2004/08/18 08:50:17 axel_schaefer no
 * message Revision 1.6 2004/07/16 08:48:01 axel_schaefer Wrong check of
 * GeoKeyDirectoryTag size. Revision 1.5 2004/07/16 07:04:53 poth no message
 * 
 * Revision 1.4 2004/07/15 15:33:43 axel_schaefer no message Revision 1.3
 * 2004/07/15 09:57:10 axel_schaefer secure saving at noontime Revision 1.2
 * 2004/07/12 08:55:48 poth no message
 * 
 * Revision 1.1 2004/07/08 15:35:22 axel_schaefer first version
 * 
 * ****************************************************************************
 */