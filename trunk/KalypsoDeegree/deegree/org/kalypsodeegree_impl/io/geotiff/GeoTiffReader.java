// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/io/geotiff/GeoTiffReader.java,v 1.4
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.apache.batik.ext.awt.image.codec.FileCacheSeekableStream;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFDecodeParam;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFDirectory;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFField;
import org.apache.batik.ext.awt.image.codec.tiff.TIFFImage;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;

/**
 * <p>
 * <tt>
 * TIFF type :: Java type<br>
 * TIFF_BYTE :: byte<br>
 * TIFF_ASCII :: String<br>
 * TIFF_SHORT :: char<br>
 * TIFF_LONG :: long<br>
 * TIFF_RATIONAL :: long[2]<br>
 * TIFF_SBYTE :: byte<br>
 * TIFF_UNDEFINED :: byte<br>
 * TIFF_SSHORT :: short<br>
 * TIFF_SLONG :: int<br>
 * TIFF_SRATIONAL :: int[2]<br>
 * TIFF_FLOAT :: float<br>
 * TIFF_DOUBLE :: double<br>
 * </tt>
 * <p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </A>
 * @author last edited by: $Author$
 * @version 2.0. $Revision$, $Date$
 * @see link to internal or external resource
 * @since
 */
public class GeoTiffReader
{

  TIFFImage image = null;

  TIFFDirectory tifdir = null;

  HashMap geoKeyDirectoryTag = null;

  boolean hasGeoKeyDirectoryTag = false;

  /**
   *  
   */
  public GeoTiffReader()
  {}

  /**
   * @param file
   * @throws FileNotFoundException
   * @throws IOException
   */
  public GeoTiffReader( File file ) throws FileNotFoundException, IOException, GeoTiffException
  {
    this();

    TIFFDecodeParam decodeParam = new TIFFDecodeParam();
    int geodirectory = 0;

    FileInputStream fis = new FileInputStream( file );
    FileCacheSeekableStream fcss = new FileCacheSeekableStream( fis );
    this.image = new TIFFImage( fcss, decodeParam, geodirectory );

    if( !isGeoTiff( this.image ) )
    {
      throw new GeoTiffException( "GeoTiffReader: TIFF is no GeoTIFF image!" );
    }

    this.tifdir = (TIFFDirectory)image.getProperty( "tiff_directory" );

    if( this.tifdir.getField( GeoTiffTag.GeoKeyDirectoryTag ) != null )
    {
      setGeoKeyDirectoryTag();
    }

  }

  // ***********************************************************************
  // General GeoTIFF tags
  // ***********************************************************************

  /**
   * <p>
   * GeoKeyDirectoryTag: <br>
   * Tag = 34735 (87AF.H) <br>
   * Type = SHORT (2-byte unsigned short) <br>
   * N = variable, >= 4 <br>
   * Alias: ProjectionInfoTag, CoordSystemInfoTag <br>
   * Owner: SPOT Image, Inc.
   * <p>
   * This tag may be used to store the GeoKey Directory, which defines and
   * references the "GeoKeys", as described below.
   * <p>
   * The tag is an array of unsigned SHORT values, which are primarily grouped
   * into blocks of 4. The first 4 values are special, and contain GeoKey
   * directory header information. The header values consist of the following
   * information, in order:
   * <p>
   * <tt>Header={KeyDirectoryVersion, KeyRevision, MinorRevision, NumberOfKeys}</tt>
   * <p>
   * and as Keys:
   * <p>
   * <tt>KeyEntry = { KeyID, TIFFTagLocation, Count, Value_Offset }</tt>^
   * <p>
   * where
   * <ul>
   * <li>"KeyID" gives the key-ID value of the Key (identical in function to
   * TIFF tag ID, but completely independent of TIFF tag-space),
   * <li>"TIFFTagLocation" indicates which TIFF tag contains the value(s) of
   * the Key: if TIFFTagLocation is 0, then the value is SHORT, and is contained
   * in the "Value_Offset" entry. Otherwise, the type (format) of the value is
   * implied by the TIFF-Type of the tag containing the value.
   * <li>"Count" indicates the number of values in this key.
   * <li>"Value_Offset" Value_Offset indicates the index- offset *into* the
   * TagArray indicated by TIFFTagLocation, if it is nonzero. If
   * TIFFTagLocation=0, then Value_Offset contains the actual (SHORT) value of
   * the Key, and Count=1 is implied. Note that the offset is not a byte-offset,
   * but rather an index based on the natural data type of the specified tag
   * array.</li>
   * </ul>
   */
  private void setGeoKeyDirectoryTag()
  {
    TIFFField ff = this.tifdir.getField( GeoTiffTag.GeoKeyDirectoryTag );

    // System.out.println("count: " + ff.getCount());

    char[] ch = ff.getAsChars();

    // resulting HashMap, containing the key and the array of values
    this.geoKeyDirectoryTag = new HashMap( ff.getCount() / 4 );
    // array of values. size is 4-1.

    int keydirversion, keyrevision, minorrevision, numberofkeys = -99;

    for( int i = 0; i < ch.length; i = i + 4 )
    {
      int[] keys = new int[3];
      keydirversion = (int)ch[i];

      keyrevision = (int)ch[i + 1];
      minorrevision = (int)ch[i + 2];
      numberofkeys = (int)ch[i + 3];
      keys[0] = keyrevision;
      keys[1] = minorrevision;
      keys[2] = numberofkeys;

      // DEBUG
      System.out.println( "[" + i + "].KEY: " + keydirversion + " \t" + keyrevision + "\t"
          + minorrevision + "\t" + numberofkeys );
      this.geoKeyDirectoryTag.put( new Integer( keydirversion ), keys );
    }
    this.hasGeoKeyDirectoryTag = true;
  }

  /**
   * <p>
   * GeoDoubleParamsTag: <br>
   * Tag = 34736 (87BO.H) <br>
   * Type = DOUBLE (IEEE Double precision) <br>
   * N = variable <br>
   * Owner: SPOT Image, Inc.
   * <p>
   * This tag is used to store all of the DOUBLE valued GeoKeys, referenced by
   * the GeoKeyDirectoryTag. The meaning of any value of this double array is
   * determined from the GeoKeyDirectoryTag reference pointing to it. FLOAT
   * values should first be converted to DOUBLE and stored here.
   * 
   * @return
   */
  public Object getGeoDoubleParamsTag()
  {
    TIFFField ff = this.tifdir.getField( GeoTiffTag.GeoDoubleParamsTag );
    // TODO GeoDoubleParamsTag
    return null;
  }

  /**
   * <p>
   * GeoAsciiParamsTag: <br>
   * Tag = 34737 (87B1.H) <br>
   * Type = ASCII <br>
   * Owner: SPOT Image, Inc. <br>
   * N = variable
   * <p>
   * This tag is used to store all of the DOUBLE valued GeoKeys, referenced by
   * the GeoKeyDirectoryTag. The meaning of any value of this double array is
   * determined from the GeoKeyDirectoryTag reference pointing to it. FLOAT
   * values should first be converted to DOUBLE and stored here.
   * <p>
   * A baseline GeoTIFF-reader must check for and convert the final "|" pipe
   * character of a key back into a NULL before returning it to the client
   * software.
   * 
   * 
   * @param image
   */
  public String[] getGeoAsciiParamsTag( int count, int value_offset )
  {

    System.out.println( "TODO: getGeoAsciiParamsTag(int count, int value_offset)!!!" );

    TIFFField field = this.tifdir.getField( GeoTiffTag.GeoAsciiParamsTag );
    String gapt = field.getAsString( 0 );

    System.out.println( gapt );

    StringTokenizer st = new StringTokenizer( gapt, "|" );

    System.out.println( st.countTokens() );

    String[] gapt_fields = new String[st.countTokens()];
    int i = 0;
    while( st.hasMoreTokens() )
    {
      gapt_fields[i++] = st.nextToken();
    }

    for( int j = 0; j < gapt_fields.length; j++ )
    {
      System.out.println( gapt_fields[j] );
    }

    return gapt_fields;
  }

  // ***********************************************************************
  // specific GeoTIFF contents GeoTIFF keys
  // ***********************************************************************
  /**
   *  
   */
  private int[] getVersionInformation() throws GeoTiffException
  {

    int[] content = new int[3];
    if( this.geoKeyDirectoryTag.containsKey( new Integer( 1 ) ) )
    {
      content = (int[])this.geoKeyDirectoryTag.get( new Integer( 1 ) );
    }
    else
    {
      throw new GeoTiffException( "No GeoTIFF Information found at Tag '1'" );
    }
    return content;
  }

  /**
   * 
   * @return fixed 1
   * @throws GeoTiffException
   */
  public int getGeoKeyDirectoryVersion() throws GeoTiffException
  {
    getVersionInformation();
    return 1;
  }

  /**
   * 
   * @return
   */
  public String getKeyRevision() throws GeoTiffException
  {
    String key_revision = "";
    int[] kv = getVersionInformation();
    key_revision = kv[0] + "." + kv[1];
    return key_revision;
  }

  public int getNumberOfKeysInGeoKeyDirectoryTag() throws GeoTiffException
  {
    int[] kv = getVersionInformation();
    return kv[2];
  }

  /**
   * <p>
   * Key ID = 1024 <br>
   * Type: SHORT (code) <br>
   * <p>
   * This GeoKey defines the general type of model Coordinate system used, and
   * to which the raster space will be transformed:unknown, Geocentric (rarely
   * used), Geographic, Projected Coordinate System, or user-defined. If the
   * coordinate system is a PCS, then only the PCS code need be specified. If
   * the coordinate system does not fit into one of the standard registered
   * PCS'S, but it uses one of the standard projections and datums, then its
   * should be documented as a PCS model with "user-defined" type, requiring the
   * specification of projection parameters, etc.
   * <p>
   * GeoKey requirements for User-Defined Model Type (not advisable):
   * GTCitationGeoKey
   * 
   * @return (0) unknown, <br>
   *         (1) ModelTypeProjected (Projection Coordinate System), <br>
   *         (2) ModelTypeGeographic (Geographic latitude-longitude System),
   *         <br>
   *         (3) ModelTypeGeocentric (Geocentric (X,Y,Z) Coordinate System)
   *         (rarely used), <br>
   *         (4?) user-defined
   * 
   * @throws GeoTiffException
   */
  public int getGTModelTypeGeoKey() throws GeoTiffException
  {
    int[] content = new int[3];
    int key = -99;

    if( this.geoKeyDirectoryTag.containsKey( new Integer( GeoTiffKey.GTModelTypeGeoKey ) ) )
    {
      content = (int[])this.geoKeyDirectoryTag.get( new Integer( GeoTiffKey.GTModelTypeGeoKey ) );

      // TIFFTagLocation
      if( content[0] == 0 )
      {
        // return Value_Offset
        key = content[2];
      }
      else
      {
        // TODO other TIFFTagLocation that GeoKeyDirectoryTag
      }
    }
    else
    {
      throw new GeoTiffException( "No GeoTIFF Information found at Tag '"
          + GeoTiffKey.GTModelTypeGeoKey + "'" );
    }
    return key;
  }

  /**
   * 
   * @throws GeoTiffException
   */
  public void getCoordinateSystem() throws GeoTiffException
  {

    if( getGTModelTypeGeoKey() == 1 )
    {
      // getModelTypeProjected();
    }
    else if( getGTModelTypeGeoKey() == 2 )
    {
      // getModelTypeGeographic();
    }
    else if( getGTModelTypeGeoKey() == 3 )
    {
      // getModelTypeGeocentric();
    }
    else
    {
      // user-defined?
    }

  }

  /**
   * 
   * @throws GeoTiffException
   */
  public GM_Envelope getBoundingBox() throws GeoTiffException
  {

    // DEBUG output
    //        for (int i = 0; i < this.tifdir.getFields().length; i++) {
    //            System.out.println(" tiff-tag: "
    //                    + this.tifdir.getFields()[i].getTag() + " \t type: "
    //                    + this.tifdir.getFields()[i].getType() + " \t count: "
    //                    + this.tifdir.getFields()[i].getCount());
    //        }

    TIFFField modelPixelScaleTag = this.tifdir.getField( GeoTiffTag.ModelPixelScaleTag );
    double resx = modelPixelScaleTag.getAsDouble( 0 );
    double resy = modelPixelScaleTag.getAsDouble( 1 );

    TIFFField modelTiepointTag = this.tifdir.getField( GeoTiffTag.ModelTiepointTag );
    double val1 = 0.0;
    val1 = modelTiepointTag.getAsDouble( 0 );
    double val2 = 0.0;
    val2 = modelTiepointTag.getAsDouble( 1 );
    double val4 = 0.0;
    val4 = modelTiepointTag.getAsDouble( 3 );
    double val5 = 0.0;
    val5 = modelTiepointTag.getAsDouble( 4 );

    if( ( resx == 0.0 || resy == 0.0 )
        || ( val1 == 0.0 && val2 == 0.0 && val4 == 0.0 && val5 == 0.0 ) )
    {
      throw new GeoTiffException( "The image/coverage hasn't a bounding box" );
      //set the geoparams derived by geoTiffTags
    }
    else
    {
      // System.out.println("val1 = " + val1 + ", val2 = " + val2
      //        + ", val4 = " + val4 + ", val5 = " + val5);

      // upper/left pixel
      double xOrigin = val4 - ( val1 * resx );
      double yOrigin = val5 - ( val2 * resy );

      // lower/right pixel
      double xRight = xOrigin + image.getWidth() * resx;
      double yBottom = yOrigin - image.getHeight() * resy;

      //            System.out.println("Resolution X: " + resx);
      //            System.out.println("Resolution Y: " + resy);
      //            System.out.println("Origin X: " + xOrigin);
      //            System.out.println("Origin Y: " + yOrigin);

      // System.out.println("image-Width = " + image.getWidth()
      //         + ", image-height = " + image.getHeight());

      double xmin = xOrigin;
      double ymin = yBottom;
      double xmax = xRight;
      double ymax = yOrigin;

      //            System.out.println("xmin = " + xmin + ", ymin = " + ymin
      //                    + ", xmax = " + xmax + ", ymax = " + ymax);

      GM_Envelope envelope = GeometryFactory.createGM_Envelope( xmin, ymin, xmax, ymax );
      return envelope;
    }
  }

  /**
   * 
   * @return
   */
  public String getHumanReadableCoordinateSystem()
  {

    String ret = "";

    if( this.geoKeyDirectoryTag.containsKey( new Integer( GeoTiffKey.PCSCitationGeoKey ) ) )
    {

      int[] key_entry = (int[])this.geoKeyDirectoryTag.get( new Integer(
          GeoTiffKey.PCSCitationGeoKey ) );

      // check if value of field is located in GeoAsciiParamsTag (34737)
      if( key_entry[0] == GeoTiffTag.GeoAsciiParamsTag )
      {
        TIFFField field = this.tifdir.getField( GeoTiffTag.GeoAsciiParamsTag );

        int ascii_length = key_entry[1];
        int ascii_start = key_entry[2];

        // return the string between the two byte-locations - 1 (the
        // last '|')
        ret = "Projected CS: " + field.getAsString( 0 ).substring( ascii_start, ascii_length - 1 );

      }
      else
      {
        ret = "value of field is NOT located in GeoAsciiParamsTag (34737).";
      }
    }
    else
    {
      ret = "<empty>";
    }

    // GeogCitationGeoKey

    return ret;
  }

  // ***********************************************************************
  // various GeoTiffReader methods
  // ***********************************************************************

  /**
   * <p>
   * description: the following TIFFKeys count as indicator if a TIFF-File
   * carries GeoTIFF information: <br>
   * ModelPixelScaleTag = 33550 (SoftDesk) <br>
   * ModelTransformationTag = 34264 (JPL Carto Group) <br>
   * ModelTiepointTag = 33922 (Intergraph) <br>
   * GeoKeyDirectoryTag = 34735 (SPOT) <br>
   * GeoDoubleParamsTag = 34736 (SPOT) <br>
   * GeoAsciiParamsTag = 34737 (SPOT)
   */
  private boolean isGeoTiff( TIFFImage image )
  {
    TIFFDirectory directory = (TIFFDirectory)image.getProperty( "tiff_directory" );

    if( directory.getField( GeoTiffTag.ModelPixelScaleTag ) == null
        && directory.getField( GeoTiffTag.ModelTransformationTag ) == null
        && directory.getField( GeoTiffTag.ModelTiepointTag ) == null
        && directory.getField( GeoTiffTag.GeoKeyDirectoryTag ) == null
        && directory.getField( GeoTiffTag.GeoDoubleParamsTag ) == null
        && directory.getField( GeoTiffTag.GeoAsciiParamsTag ) == null )
    {
      return false;
    }
    else
    {
      return true;
    }
  }

  /**
   * 
   * @param file
   * @return
   */
  public TIFFImage getTIFFImage() throws GeoTiffException
  {
    if( this.image != null )
    {
      return this.image;
    }
    else
    {
      throw new GeoTiffException( "no image" );
    }

  }

  /**
   *  
   */
  public String toString()
  {
    String ret = "GeoTIFF Information:\n";

    if( hasGeoKeyDirectoryTag )
    {

      // *** Version Information
      try
      {
        ret += "  Version: " + getGeoKeyDirectoryVersion() + "\n";
        ret += "  Key_Revision: " + getKeyRevision() + "\n";
        ret += "  Number Of Keys in GeoKeyDirectoryTag: " + getNumberOfKeysInGeoKeyDirectoryTag()
            + "\n";

        ret += "  GTModelTypeGeoKey: " + getGTModelTypeGeoKey() + "\n";

      }
      catch( GeoTiffException e )
      {
        ret += "GeoTiffException occured when requesting GeoTIFF Version Information:\n"
            + e.getMessage();
      }

      // ***
      ret += "\n";

      ret += "Coordinate System (human readable): " + getHumanReadableCoordinateSystem() + "\n";

      ret += "\n";
      // ***

    }
    else
    {
      ret += "\nNo GeoKeyDirectoryTag (34735) specified.\n";
    }

    if( this.tifdir.getField( GeoTiffTag.ModelPixelScaleTag ) != null
        || this.tifdir.getField( GeoTiffTag.ModelTiepointTag ) != null )
    {

      ret += "Corner Coordinates:\n";
      GM_Envelope envelope;
      try
      {
        envelope = getBoundingBox();
        ret += "  Upper Left ( " + envelope.getMin().getX() + ", " + envelope.getMax().getY()
            + " )\n" + "  Lower Left ( " + envelope.getMin().getX() + ", "
            + envelope.getMin().getY() + " )\n" + "  Upper Right ( " + envelope.getMax().getX()
            + ", " + envelope.getMax().getY() + " )\n" + "  Lower Right ( "
            + envelope.getMax().getX() + ", " + envelope.getMin().getY() + " )\n";
      }
      catch( GeoTiffException e )
      {
        ret += "GeoTiffException occured when calculation the BoundingBox:\n" + e.getMessage();
      }
    }
    else
    {
      ret += "\nNo BoundingBox Information in ModelPixelScaleTag (33550) and  ModelTiepointTag (33922).\n"
          + "ModelTransformationTag (34264) not implemented. \n"
          + "Here is a list of the available tags:\n";

      for( int i = 0; i < this.tifdir.getFields().length; i++ )
      {
        ret += "  tag: " + this.tifdir.getFields()[i].getTag() + " \t type: "
            + this.tifdir.getFields()[i].getType() + " \t count: "
            + this.tifdir.getFields()[i].getCount() + "\n";
      }

    }

    return ret;
  }

  // ***********************************************************************
  // main-method
  // ***********************************************************************

  /**
   * 
   * @param args
   */
  public static void main( String[] args )
  {
    // intergraph/output.tif
    // chicago/SP27GTIF.jpg
    String stringfile = "d:/geotiff/intergraph/output.tif";

    try
    {
      GeoTiffReader gtr = new GeoTiffReader( new File( stringfile ) );
      // gtr.getBoundingBox(tifdir);
      // gtr.getGeoAsciiParamsTag(tifdir);
      // gtr.getGeoKeyDirectoryTag();

      // gtr.getCoordinateSystem();

      System.out.println( gtr.toString() );
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
 * empty log message *** Revision 1.4 2004/08/18 08:50:17 axel_schaefer no
 * message
 * 
 * Revision 1.3 2004/07/15 15:33:43 axel_schaefer no message
 * 
 * Revision 1.2 2004/07/15 09:57:23 axel_schaefer no message
 * 
 * ****************************************************************************
 */