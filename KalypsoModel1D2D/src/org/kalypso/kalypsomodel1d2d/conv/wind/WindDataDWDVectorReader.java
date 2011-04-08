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
package org.kalypso.kalypsomodel1d2d.conv.wind;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

import org.deegree.framework.util.Pair;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * <p>
 * Reader for DWD-Files, thus are ASCII container files with wind data for some regular grid and many time steps.
 * </p>
 * <p>
 * the header of the file contains the description of following wind data, the lines of the header are parsed according
 * to the standard order
 * </p>
 * <p>
 * the data will be separated in time-step parts with key word "termin" and inside of this steps by the key word "line"
 * for corresponding part of the grid.
 * </p>
 * 
 * One source DWD-file will be read and for each time step one binary grid of type {@link BinaryGeoGridPairsValues} will
 * be written in the target directory.
 * 
 * according to {@link IWindDataCollectionReader} interface this class provides the collection of read and written data
 * steps
 * 
 * 
 * 
 * @author ig
 * 
 */
@SuppressWarnings( {"rawtypes", "unchecked"} )
public class WindDataDWDVectorReader implements IWindDataCollectionReader
{
  // next data is available in source DWD file, but is still unused in actual implementation case
  private GM_Point m_gmGridStartPoint;

  @SuppressWarnings("unused")
  private String m_strFileTypeDWDLine = ""; //$NON-NLS-1$

  @SuppressWarnings("unused")
  private int m_nodeIdDWD;

  @SuppressWarnings("unused")
  private String m_strStationNameDWD;

  private int m_intNumberOfElementProperties;

  /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  private Pair<Double, Double> m_pairsActTimeStepWindData[][];

  private double m_doubleCellSizeX;

  private double m_doubleCellSizeY;

  private RectifiedGridDomain m_gridDescriptor;

  private List<IWindDataWrapper> m_listWindDataProviders;

  private Map<Date, Pair[][]> m_mapAllSteps;

  private List<String> m_listComments;

  private int m_intNumberColumns;

  private int m_intNumberRows;

  private URL m_urlInputFile;

  private URL m_urlOutputDir;

  private String m_strFileNameSuffix;

  private BufferedReader m_inputReader;

  private String m_strTimeStepLine = ""; //$NON-NLS-1$

  private String m_strSourceCrs = "";

  private List<Integer> m_listElementPropId;

  private List<String> m_listTimeAdditionalInfo;

  private Date m_dateActStep;

  private int m_intReadFirstElements = 0;

  private int m_intReadSecondElements = 0;

  private static final Logger logger = Logger.getLogger( WindDataDWDVectorReader.class.getName() );

  private static final String STR_PREFIX_TIME_STEP = "termin"; //$NON-NLS-1$

  private static final String STR_PREFIX_COMMENT = "c"; //$NON-NLS-1$

  private static final String STR_PREFIX_DWD_TYPE = "#"; //$NON-NLS-1$

  private static final String STR_PREFIX_LINE = "line"; //$NON-NLS-1$

  private static final Object STR_END_FILE = "-1"; //$NON-NLS-1$

  public WindDataDWDVectorReader( )
  {
    logger.info( "WindDataReaderDWDVectorGeneric::default constructor" ); //$NON-NLS-1$
  }

  public WindDataDWDVectorReader( final URL urlInputFileName, final URL urlOutputDir, final String strFileNameSuffix, final String pSelectedCoordinateSystem )
  {
    m_urlInputFile = urlInputFileName;
    m_urlOutputDir = urlOutputDir;
    m_strFileNameSuffix = strFileNameSuffix;
    m_strSourceCrs = pSelectedCoordinateSystem;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IWindDataReader#read()
   */
  @Override
  public boolean read( )
  {
    boolean lBoolRes = false;
    try
    {
      InputStream lUrlStream = null;
      InputStream lBufferedInputStream = null;
      InputStream lInputStream = null;

      lUrlStream = m_urlInputFile.openStream();
      lBufferedInputStream = new BufferedInputStream( lUrlStream );

      if( m_urlInputFile.toExternalForm().endsWith( ".gz" ) ) //$NON-NLS-1$
      {
        lInputStream = new GZIPInputStream( lBufferedInputStream );
      }
      else if( m_urlInputFile.toExternalForm().endsWith( ".zip" ) ) //$NON-NLS-1$
      {
        lInputStream = ZipUtilities.getInputStreamForFirstFile( m_urlInputFile );
      }
      else
      {
        lInputStream = lBufferedInputStream;
      }

      InputStreamReader lInputStreamReader = new InputStreamReader( lInputStream );
      m_inputReader = new BufferedReader( lInputStreamReader );

      lBoolRes = readHeader();
      lBoolRes = lBoolRes & readContent();

      m_inputReader.close();
      lInputStreamReader.close();
      lInputStream.close();
      lBufferedInputStream.close();
      lUrlStream.close();

    }
    catch( Exception e )
    {
      e.printStackTrace();
      logger.warning( "cannot convert wind data source file to internal format" + e ); //$NON-NLS-1$
      lBoolRes = false;
    }
    return lBoolRes;
  }

  private boolean readContent( ) throws IOException
  {
    boolean lBoolRes = true;
    m_mapAllSteps = new HashMap<Date, Pair[][]>();
    m_listWindDataProviders = new ArrayList<IWindDataWrapper>();

    // the first time step line was read in the header reader block, else format error
    if( m_strTimeStepLine.equals( "" ) ) { //$NON-NLS-1$
      return false;
    }
    m_dateActStep = parseDateLine();
    String lStrActLine = ""; //$NON-NLS-1$
    ActTimeStepLineDescriptor lActBlockDescriptor = null;
    try
    {
      lStrActLine = m_inputReader.readLine().trim().toLowerCase();
    }
    catch( IOException e )
    {
      e.printStackTrace();
      lBoolRes = false;
    }
    while( true )
    {
      if( lStrActLine.startsWith( STR_PREFIX_TIME_STEP ) )
      {
        m_mapAllSteps.put( m_dateActStep, m_pairsActTimeStepWindData );
        WindDataGenericGridConverter lActWindDataProvider = new WindDataGenericGridConverter( m_dateActStep, m_pairsActTimeStepWindData, m_gridDescriptor, m_urlOutputDir, m_strFileNameSuffix );
        if( !lActWindDataProvider.convert() )
        {
          throw new IOException( "Cannot convert actual data!" );
        }
        m_listWindDataProviders.add( lActWindDataProvider );
        initializePairsArray();
        m_strTimeStepLine = lStrActLine;
        m_dateActStep = parseDateLine();
      }
      else if( lStrActLine.startsWith( STR_PREFIX_LINE ) )
      {
        lActBlockDescriptor = new ActTimeStepLineDescriptor( lStrActLine );
        m_intReadFirstElements = 0;
        m_intReadSecondElements = 0;
      }
      else if( m_intReadFirstElements == lActBlockDescriptor.m_intActBlockAmount && m_intReadSecondElements == lActBlockDescriptor.m_intActBlockAmount )
      {
        if( lStrActLine.length() == 0 )
        {
          continue;
        }
        else
        {
          if( lStrActLine.equals( STR_END_FILE ) || !m_inputReader.ready() )
          {
            break;
          }
        }
      }
      else
      {
        parseDataLine( lStrActLine, lActBlockDescriptor );
      }
      try
      {
        lStrActLine = m_inputReader.readLine().trim().toLowerCase();
      }
      catch( IOException e )
      {
        e.printStackTrace();
        lBoolRes = false;
      }
    }
    return lBoolRes;
  }

  private void parseDataLine( String strActLine, ActTimeStepLineDescriptor actBlockDescriptor )
  {
    StringTokenizer lStringTokenizer = new StringTokenizer( strActLine, " " ); //$NON-NLS-1$
    while( lStringTokenizer.hasMoreTokens() && (m_intReadFirstElements < actBlockDescriptor.m_intActBlockAmount || m_intReadSecondElements < actBlockDescriptor.m_intActBlockAmount) )
    {
      if( m_intReadFirstElements < actBlockDescriptor.m_intActBlockAmount )
      {
        m_pairsActTimeStepWindData[m_intReadFirstElements++ + actBlockDescriptor.m_intActBlockColumnStart][m_intNumberRows - actBlockDescriptor.m_intActBlockLine].first = Double.parseDouble( lStringTokenizer.nextToken() );
      }
      else
      {
        m_pairsActTimeStepWindData[m_intReadSecondElements++ + actBlockDescriptor.m_intActBlockColumnStart][m_intNumberRows - actBlockDescriptor.m_intActBlockLine].second = Double.parseDouble( lStringTokenizer.nextToken() );
      }
    }
  }

  private Date parseDateLine( )
  {
    SimpleDateFormat lDateFormatter;
    lDateFormatter = new SimpleDateFormat( "dd.MM.yyyy HH mm ss" ); //$NON-NLS-1$
    StringTokenizer lStringTokenizer = new StringTokenizer( m_strTimeStepLine, "=" ); //$NON-NLS-1$
    lStringTokenizer.nextToken();
    Date lDateRes = null;
    try
    {
      lDateRes = lDateFormatter.parse( lStringTokenizer.nextToken() );
    }
    catch( ParseException e )
    {
      e.printStackTrace();
    }
    return lDateRes;
  }

  private boolean readHeader( )
  {
    boolean lBoolRes = true;

    m_listElementPropId = new ArrayList<Integer>();
    m_listTimeAdditionalInfo = new ArrayList<String>();
    m_listComments = new ArrayList<String>();
    int lIntDataInputsCounter = 0;
    while( true )
    {
      String lStrActLine = ""; //$NON-NLS-1$
      try
      {
        lStrActLine = m_inputReader.readLine().trim().toLowerCase();
        if( lStrActLine.startsWith( STR_PREFIX_COMMENT ) )
        {
          m_listComments.add( lStrActLine );
        }
        else if( lStrActLine.startsWith( STR_PREFIX_TIME_STEP ) )
        {
          m_strTimeStepLine = lStrActLine;
          break;
          // lBoolContinue = false;
        }
        else if( lStrActLine.startsWith( STR_PREFIX_DWD_TYPE ) )
        {
          m_strTimeStepLine = lStrActLine;
        }
        else
        {
          switch( lIntDataInputsCounter )
          {
            case 0:
              m_nodeIdDWD = Integer.parseInt( lStrActLine );
              break;
            case 1:
              m_strStationNameDWD = lStrActLine;
              break;
            case 2:
              StringTokenizer lStrTokenizerPositions = new StringTokenizer( lStrActLine, " \t" ); //$NON-NLS-1$
              setGmGridStartPoint( GeometryFactory.createGM_Point( Double.parseDouble( lStrTokenizerPositions.nextToken() ), Double.parseDouble( lStrTokenizerPositions.nextToken() ), m_strSourceCrs ) );
              break;
            case 3:
              StringTokenizer lStrTokenizerGridData = new StringTokenizer( lStrActLine, " \t" ); //$NON-NLS-1$
              m_intNumberColumns = Integer.parseInt( lStrTokenizerGridData.nextToken() );
              m_intNumberRows = Integer.parseInt( lStrTokenizerGridData.nextToken() );
              m_doubleCellSizeX = Double.parseDouble( lStrTokenizerGridData.nextToken() );
              m_doubleCellSizeY = Double.parseDouble( lStrTokenizerGridData.nextToken() );
              break;
            case 4:
              StringTokenizer lStrTokenizerElementDataAsStrings = new StringTokenizer( lStrActLine, " \t" ); //$NON-NLS-1$
              // TODO: specification of wind-DWD format - is it possible to have here more then 2 characteristics for
              // wind?
              m_intNumberOfElementProperties = Integer.parseInt( lStrTokenizerElementDataAsStrings.nextToken() );
              for( int i = 0; i < m_intNumberOfElementProperties; ++i )
                m_listElementPropId.add( Integer.parseInt( lStrTokenizerElementDataAsStrings.nextToken() ) );
              break;
            case 5:
              StringTokenizer lStrTokenizerTimeAdditionlInfoAsStrings = new StringTokenizer( lStrActLine, " \t" ); //$NON-NLS-1$
              int lIntCountTokens = lStrTokenizerTimeAdditionlInfoAsStrings.countTokens();
              // TODO: specification of wind-DWD format - what additional factors can be set to start time?
              for( int i = 0; i < lIntCountTokens; ++i )
                m_listTimeAdditionalInfo.add( lStrTokenizerTimeAdditionlInfoAsStrings.nextToken() );
              break;
            default:
              logger.warning( "format error, line ignored: " + lStrActLine ); //$NON-NLS-1$

          }
          lIntDataInputsCounter++;
        }

      }
      catch( IOException e )
      {
        e.printStackTrace();
        // lBoolContinue = false;
        lBoolRes = false;
      }
    }
    m_pairsActTimeStepWindData = new Pair[m_intNumberColumns][m_intNumberRows];

    final OffsetVector offsetX = new OffsetVector( m_doubleCellSizeX, 0 );
    final OffsetVector offsetY = new OffsetVector( 0, -m_doubleCellSizeY );

    final double[] low = { 0.0, 0.0 };
    final double[] high = { m_intNumberColumns, m_intNumberRows };
    final GridRange gridRange = new GridRange_Impl( low, high );

    // important! DWD grid starts in left upper corner and kalypso grid starts with lower left corner.
    setGmGridStartPoint( GeometryFactory.createGM_Point( m_gmGridStartPoint.getX(), m_gmGridStartPoint.getY() - m_doubleCellSizeY * m_intNumberRows, m_strSourceCrs ) );

    try
    {
      m_gridDescriptor = new RectifiedGridDomain( m_gmGridStartPoint, offsetX, offsetY, gridRange );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    initializePairsArray();
    return lBoolRes;
  }

  private void initializePairsArray( )
  {
    for( int i = 0; i < m_intNumberColumns; ++i )
    {
      for( int j = 0; j < m_intNumberRows; ++j )
      {
        m_pairsActTimeStepWindData[i][j] = new Pair<Double, Double>( Double.NaN, Double.NaN );
      }
    }

  }

  public final List<String> getListComments( )
  {
    return m_listComments;
  }

  public final List<Integer> getListElementPropId( )
  {
    return m_listElementPropId;
  }

  public final List<String> getListTimeAdditionalInfo( )
  {
    return m_listTimeAdditionalInfo;
  }

  private void setGmGridStartPoint( GM_Point gmGridStartPoint )
  {
    m_gmGridStartPoint = gmGridStartPoint;
  }

  private class ActTimeStepLineDescriptor
  {
    public int m_intActBlockLine = 0;

    public int m_intActBlockColumnStart = 0;

    @SuppressWarnings("unused")
    public int m_intActBlockColumnEnd = 0;

    public int m_intActBlockAmount = 0;

//    public ActTimeStepLineDescriptor( )
//    {
//    }

    public ActTimeStepLineDescriptor( final String pStrRawLine )
    {
      StringTokenizer lStringTokenizer = new StringTokenizer( pStrRawLine, " =\t" ); //$NON-NLS-1$
      lStringTokenizer.nextToken();
      m_intActBlockLine = Integer.parseInt( lStringTokenizer.nextToken() );
      lStringTokenizer.nextToken();
      lStringTokenizer.nextToken();
      lStringTokenizer.nextToken();
      m_intActBlockAmount = Integer.parseInt( lStringTokenizer.nextToken() );
      lStringTokenizer.nextToken();
      m_intActBlockColumnStart = Integer.parseInt( lStringTokenizer.nextToken() );
      lStringTokenizer.nextToken();
      m_intActBlockColumnEnd = Integer.parseInt( lStringTokenizer.nextToken() );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataCollectionProvider#getCollectionWindDataProviders()
   */
  @Override
  public List<IWindDataWrapper> getCollectionWindDataProviders( )
  {
    return m_listWindDataProviders;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.wind.IWindDataCollectionProvider#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( )
  {
    return m_gridDescriptor;
  }

}
