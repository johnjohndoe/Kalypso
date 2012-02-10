/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.vfs.FileObject;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.jmatio.io.MatFileFilter;
import com.jmatio.io.MatFileReader;
import com.jmatio.io.MatlabIOException;
import com.jmatio.types.MLArray;
import com.jmatio.types.MLNumericArray;

/**
 * @author ig
 * 
 *         Reads SWAN results output MATLAB file. This file is according to related SWAN-version is MATLAB version 4
 *         binary file. The reading operation is based on extended JMatIO library, which is an open source project
 *         originally written by Wojciech Gradkowski (<a href="mailto:wgradkowski@gmail.com">wgradkowski@gmail.com</a>),
 *         and can be found on {@link http://sourceforge.net/projects/jmatio/develop}
 */
public class SWANResultsReader
{
  private String m_strResultsFile;

  private FileObject m_resultsFile;

  public final static String STR_XP_NAME = "Xp"; //$NON-NLS-1$

  public final static String STR_YP_NAME = "Yp"; //$NON-NLS-1$

  public static final int INT_ROUND_SIGNIFICANT = 3;

  private static double m_doubleShiftX;

  private static double m_doubleShiftY;

  /**
   * Constructor with full results file name as {@link String} parameter
   */
  public SWANResultsReader( final String pStrResultsFile )
  {
    m_strResultsFile = pStrResultsFile;
    m_resultsFile = null;
  }

  /**
   * Constructor with full results file name as {@link String} parameter
   */
  public SWANResultsReader( final FileObject pResultsFile )
  {
    m_strResultsFile = null;// pResultsFile.getName().getBaseName();
    m_resultsFile = pResultsFile;
  }

  /**
   * This function reads the given SWAN results MATLAB file and formats the data in the {@link Map} with the
   * {@link String} result's matrixes names(in lower case) as keys and values from according matrixes also as a
   * {@link Map} with corresponding {@link GM_Position} as keys. The positions will be shifted by given values, that
   * should be read from default file
   * 
   * @return Map< String, Map< GM_Position, Double > > with data from results file, null in case of error
   * 
   */
  public Map<String, Map<GM_Position, Double>> readMatResultsFile( )
  {
    return readMatResultsFile( null );
  }

  public Map<String, Map<GM_Position, Double>> readMatResultsFile( final String pStrFilter )
  {
    MatFileReader lMatFileReader = null;
    MatFileFilter lMatFilter = new MatFileFilter();
    Map<String, Map<GM_Position, Double>> lRes = null;
    try
    {
      if( pStrFilter != null )
      {
        lMatFilter.addArrayName( STR_XP_NAME );
        lMatFilter.addArrayName( STR_YP_NAME );
        lMatFilter.addArrayName( pStrFilter );
      }
      if( m_strResultsFile == null )
      {
        lMatFileReader = new MatFileReader( FileUtils.toFile( m_resultsFile.getURL() ), lMatFilter );
      }
      else
      {
        lMatFileReader = new MatFileReader( m_strResultsFile, lMatFilter );
      }
    }
    catch( Exception e )
    {
      // throw new MatlabIOException( "Cannot open or read SWAN result matlab file." );
    }
    if( lMatFileReader != null )
    {
      try
      {
        GM_Position lShiftPosition = SWANDataConverterHelper.readCoordinateShiftValues( m_resultsFile );
        m_doubleShiftX = lShiftPosition.getX();
        m_doubleShiftY = lShiftPosition.getY();
        Map<String, MLArray> lMapData = lMatFileReader.getContent();
        // printDebugParsedSWANRawData( lMatFileReader, null );
        lRes = getValuesFormatedNameDatePosition( lMapData );
        // printDebugResultData( lRes, null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    return lRes;
  }

  // debug only: raw text output of results
  @SuppressWarnings("unused")
  private static void printDebugParsedSWANRawData( final MatFileReader mfr, final String outputPath ) throws IOException, MatlabIOException
  {
    String outputPathTmp = "d:/temp/MatReaderOut-"; //$NON-NLS-1$
    if( outputPath != null && !"".equals( outputPath ) )
    {
      outputPathTmp = outputPath;
    }
    Map<String, MLArray> lMapData = mfr.getContent();
    FileWriter fstream = new FileWriter( outputPathTmp + (new Date()).getTime() );
    BufferedWriter out = new BufferedWriter( fstream );
    Set<String> lSetKeys = lMapData.keySet();
    for( final String lStrKey : lSetKeys )
    {
      MLArray lValues = lMapData.get( lStrKey );
      out.write( lValues.contentToString() );
    }
    out.close();
  }

  // debug only: raw text output of results
  @SuppressWarnings("unused")
  private static void printDebugResultData( Map<String, Map<GM_Position, Double>> pRes, final String outputPath ) throws IOException, MatlabIOException
  {
    String outputPathTmp = "d:/temp/MatReaderOutRes-"; //$NON-NLS-1$
    if( outputPath != null && !"".equals( outputPath ) )
    {
      outputPathTmp = outputPath;
    }
    FileWriter fstream = new FileWriter( outputPathTmp + (new Date()).getTime() );
    BufferedWriter out = new BufferedWriter( fstream );
    Set<String> lSetKeys = pRes.keySet();
    int iCount = 0;
    for( final String lStrKey : lSetKeys )
    {
      out.write( " " + lStrKey );

      Map<GM_Position, Double> lValues = pRes.get( lStrKey );
      Set<GM_Position> lSetKeysData = lValues.keySet();
      for( Iterator<GM_Position> iterator = lSetKeysData.iterator(); iterator.hasNext(); )
      {
        GM_Position gmPosition = iterator.next();
        out.write( "gm_p: " + gmPosition );

        out.write( "value:: " + lValues.get( gmPosition ) + ", count: " + iCount++ + ";" );
      }
      out.write( "\n" );
    }
    // out.write( lValues.contentToString() );
    out.close();
  }

  /**
   * This function returns the data formated as map with a keys as {@link String} which are equal to the original names
   * of each result matrix in SWAN result file, the set of values is a map with positions as keys and corresponding
   * values of the matrix with the given name from original result file.
   * 
   * @return Map<String, Map<GM_Position, Double>>
   */
  private static Map<String, Map<GM_Position, Double>> getValuesFormatedNameDatePosition( final Map<String, MLArray> pMLArrayData ) throws Exception
  {
    Map<String, Map<GM_Position, Double>> lMapResult = new HashMap<String, Map<GM_Position, Double>>();
    List<Double> lListX;
    List<Double> lListY;
    List<GM_Position> lListGM_Positions;
    Set<String> lSetKeys = pMLArrayData.keySet();

    lListX = getValuesAsList( pMLArrayData.get( STR_XP_NAME ) );
    lListY = getValuesAsList( pMLArrayData.get( STR_YP_NAME ) );

    int lIntAmountCoordinates = lListX.size();

    if( lIntAmountCoordinates != lListY.size() )
    {
      throw new Exception( "Incorrect SWAN output as matlab array class(Level4): amount of x-coordinates and y-coordinates are not equal " );
    }
    lListGM_Positions = createListOfPositions( lListX, lListY );
    for( final String lStrKey : lSetKeys )
    {
      if( lStrKey.equalsIgnoreCase( STR_XP_NAME ) || lStrKey.equalsIgnoreCase( STR_YP_NAME ) )
      {
        continue;
      }
      List<Double> lListValuesAct = getValuesAsList( pMLArrayData.get( lStrKey ) );

      if( lIntAmountCoordinates != lListValuesAct.size() )
      {
        throw new Exception( "Incorrect SWAN output as matlab array class(Level4): amount of coordinates and data are not equal " );
      }
      lMapResult.put( lStrKey.toLowerCase(), createMapWithCoordinates( lListGM_Positions, lListValuesAct ) );

    }
    return lMapResult;

  }

  /**
   * creating list of positions {@link GM_Position} from the given lists of {@link Double}
   * 
   * @return List<GM_Position>
   */
  private static List<GM_Position> createListOfPositions( final List<Double> pListX, final List<Double> pListY )
  {
    List<GM_Position> lListRes = new ArrayList<GM_Position>();
    int lIntAmountCoordinates = pListX.size();

    for( int i = 0; i < lIntAmountCoordinates; ++i )
    {
      lListRes.add( GeometryFactory.createGM_Position( NumberUtils.getRoundedToSignificant( pListX.get( i ) + m_doubleShiftX, INT_ROUND_SIGNIFICANT ), NumberUtils.getRoundedToSignificant( pListY.get( i )
          + m_doubleShiftY, INT_ROUND_SIGNIFICANT ) ) );
    }
    return lListRes;
  }

  /**
   * creating map with positions {@link GM_Position} as keys and values from given list
   * 
   * @return Map<GM_Position, Double>
   */
  private static Map<GM_Position, Double> createMapWithCoordinates( final List<GM_Position> pListPostitions, final List<Double> pListValuesAct )
  {
    Map<GM_Position, Double> lMapResult = new HashMap<GM_Position, Double>();
    int lIntAmountCoordinates = pListPostitions.size();

    for( int i = 0; i < lIntAmountCoordinates; ++i )
    {
      lMapResult.put( pListPostitions.get( i ), pListValuesAct.get( i ) );
    }
    return lMapResult;
  }

  /**
   * getting values as {@link Double} from given as {@link MLArray} matlab matrix
   * 
   * @return List<Double>
   */
  private static List<Double> getValuesAsList( final MLArray pMLArray ) throws Exception
  {
    List<Double> lListRes = new ArrayList<Double>();
    if( pMLArray == null )
    {
      throw new Exception( "Incorrect SWAN output as matlab array class(Level4): data information has incorrect precision " );
    }
    MLNumericArray lMLArray = (MLNumericArray) pMLArray;

    for( int m = 0; m < lMLArray.getM(); m++ )
    {
      for( int n = 0; n < lMLArray.getN(); n++ )
      {
        lListRes.add( (lMLArray.get( m, n )).doubleValue() );
      }
    }
    return lListRes;
  }

  public final String getStrResultsDir( )
  {
    return m_strResultsFile;
  }

  public final void setStrResultsDir( String strResultsDir )
  {
    m_strResultsFile = strResultsDir;
  }

}
