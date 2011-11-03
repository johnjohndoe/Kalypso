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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.vfs.FileObject;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.conv.results.SimpleNodeResultsHandler;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;

/**
 * @author ig
 */
public class SWANAdditionalDataConverter
{
  /** Directory name for RMA∑Kalypso result files (Output...) files */
  public static final String RESULT_DIR_NAME = "./"; //$NON-NLS-1$

  public static final String WATER_LEVEL_SERIES_NAMES_KEY = "waterLevelBlockFiles"; //$NON-NLS-1$

  public static final String CURRENTS_SERIES_NAMES_KEY = "currentsBlockFiles"; //$NON-NLS-1$

  private final SimpleNodeResultsHandler m_resultsSimpleHandler;

  private final FileObject m_fileObjWorkingDir;

  private final ResultManager m_resultManager;

  private final Date[] m_arrDatesToProceed;

  private boolean m_boolMultipleSteps = false;

  private Date[] m_calculatedSteps;

  private String m_strStepLength = ""; //$NON-NLS-1$

  private String m_strDefaulCurrentValue = "0.0"; //$NON-NLS-1$

  private Map<String, List<String>> m_mapWrittenFilesNames;

  public SWANAdditionalDataConverter( final ResultManager pResultManager, final SimpleNodeResultsHandler pResultsSimpleHandler, final FileObject pFileObjWorkingDir, final Date[] pArrDatesToProceed )
  {
    m_resultManager = pResultManager;
    m_resultsSimpleHandler = pResultsSimpleHandler;
    m_fileObjWorkingDir = pFileObjWorkingDir;
    m_arrDatesToProceed = pArrDatesToProceed;
    m_mapWrittenFilesNames = new HashMap<String, List<String>>();

    try
    {
      m_calculatedSteps = m_resultManager.findCalculatedSteps();
      if( m_calculatedSteps.length > 1 )
      {
        m_boolMultipleSteps = true;
        m_calculatedSteps = removeSteadyDates( m_calculatedSteps, null );
      }
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    if( m_boolMultipleSteps && m_calculatedSteps.length > 1 )
    {
      // step length in minutes
      m_strStepLength = "" + ((m_calculatedSteps[1].getTime() - m_calculatedSteps[0].getTime()) / 60000); //$NON-NLS-1$
    }

  }

  public static Date[] removeSteadyDates( Date[] pArrayDates, final Date pDate )
  {
    if( pArrayDates == null || pArrayDates.length == 1 )
    {
      return null;
    }
    List<Date> lListDatesToRemove = new ArrayList<Date>();
    if( pDate == null )
    {
      lListDatesToRemove.add( ISimulation1D2DConstants.STEADY_DATE );
      lListDatesToRemove.add( ISimulation1D2DConstants.MAXI_DATE );
    }
    else
    {
      lListDatesToRemove.add( pDate );
    }
    List<Date> lListDatesRes = new ArrayList<Date>();
    for( int i = 0; i < pArrayDates.length; ++i )
    {
      if( !lListDatesToRemove.contains( pArrayDates[i] ) )
      {
        lListDatesRes.add( pArrayDates[i] );
      }
    }
    return lListDatesRes.toArray( new Date[lListDatesRes.size()] );
  }

  /**
   * 
   * returns map with lists of written water files, associative keys are WATER_LEVEL_SERIES_NAMES_KEY and
   * CURRENTS_SERIES_NAMES_KEY
   * 
   */
  public Map<String, List<String>> writeDataFiles( )
  {
    writeWaterBlock();
    return m_mapWrittenFilesNames;
  }

  private void writeWaterBlock( )
  {
    String lStrFileNameWLData = ISimulation1D2DConstants.SIM_SWAN_WATER_LEVEL_DATA_FILE;
    String lStrFileNameCurrentData = ISimulation1D2DConstants.SIM_SWAN_CURRENT_DATA_FILE;
    List<String> lListWLFilesNames = new ArrayList<String>();
    List<String> lListCurrentFilesNames = new ArrayList<String>();
    FileObject lModelWLFile = null;
    FileObject lModelCurrentFile = null;
    try
    {
      if( m_boolMultipleSteps )
      {
        lStrFileNameWLData = lStrFileNameWLData + ISimulation1D2DConstants.SIM_SWAN_TIME_SUFFIX + m_strStepLength;
        lStrFileNameCurrentData = lStrFileNameCurrentData + ISimulation1D2DConstants.SIM_SWAN_TIME_SUFFIX + m_strStepLength;
      }
      lStrFileNameWLData += ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
      lStrFileNameCurrentData += ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
      lModelWLFile = m_fileObjWorkingDir.resolveFile( lStrFileNameWLData );
      lModelCurrentFile = m_fileObjWorkingDir.resolveFile( lStrFileNameCurrentData );

      if( m_boolMultipleSteps )
      {
        for( int i = 0; i < m_arrDatesToProceed.length; ++i )
        {
          String lStrActWLSeriesFileName = getActSeriesFileName( ISimulation1D2DConstants.SIM_SWAN_WATER_LEVEL_SERIES_FILE, m_arrDatesToProceed[i] ) + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
          String lStrActCurrentSeriesFileName = getActSeriesFileName( ISimulation1D2DConstants.SIM_SWAN_CURRENT_SERIES_FILE, m_arrDatesToProceed[i] ) + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
          final FileObject lModelWLFileSerie = m_fileObjWorkingDir.resolveFile( lStrActWLSeriesFileName );
          final FileObject lModelCurrentFileSerie = m_fileObjWorkingDir.resolveFile( lStrActCurrentSeriesFileName );

          writeWLSeriesFile( m_arrDatesToProceed[i], lModelWLFileSerie );
          writeCurrentSeriesFile( m_arrDatesToProceed[i], lModelCurrentFileSerie );

          lListWLFilesNames.add( lStrActWLSeriesFileName );
          lListCurrentFilesNames.add( lStrActCurrentSeriesFileName );
        }
        m_mapWrittenFilesNames.put( CURRENTS_SERIES_NAMES_KEY, lListCurrentFilesNames );
        m_mapWrittenFilesNames.put( WATER_LEVEL_SERIES_NAMES_KEY, lListWLFilesNames );
      }
      else
      {
        if( m_arrDatesToProceed.length > 0 )
        {
          writeWLSeriesFile( m_arrDatesToProceed[0], lModelWLFile );
          writeCurrentSeriesFile( m_arrDatesToProceed[0], lModelCurrentFile );
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {

    }

  }

  private void writeCurrentSeriesFile( Date pDate, FileObject modelCurrentFileSerie )
  {
    Formatter lFormatterCurrent = null;
    // put first the Y component of current into buffer to write it out after X-component according to SWAN formating
    // rules
    StringBuffer lStrBuffY = new StringBuffer();
    try
    {
      List<INodeResult> lListActResults = m_resultsSimpleHandler.getResultsForDate( pDate );
      lFormatterCurrent = new Formatter( modelCurrentFileSerie.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      if( lListActResults == null )
      {
        // TODO: debug output...
        return;
      }
      double lDoubleExclNr = Double.parseDouble( ISimulation1D2DConstants.SIM_SWAN_EXCLUSION_NUMBER );
      for( final INodeResult lResultAct : lListActResults )
      {
        if( lDoubleExclNr != lResultAct.getWaterlevel() )
        {
          try
          {
            List<Double> lListDoubleVelocity = lResultAct.getVelocity();// AbsoluteVelocity();
            lFormatterCurrent.format( "%.2f\n", lListDoubleVelocity.get( 0 ) ); //$NON-NLS-1$
            lStrBuffY.append( String.format( Locale.US, "%.2f\n", lListDoubleVelocity.get( 1 ) ) );
          }
          catch( Exception e )
          {
            lFormatterCurrent.format( "%s\n", m_strDefaulCurrentValue ); //$NON-NLS-1$
            lStrBuffY.append( String.format( Locale.US, "%s\n", m_strDefaulCurrentValue ) );
          }
        }
        else
        {
          lFormatterCurrent.format( "%s\n", m_strDefaulCurrentValue ); //$NON-NLS-1$
          lStrBuffY.append( String.format( Locale.US, "%s\n", m_strDefaulCurrentValue ) );
        }
      }
      lFormatterCurrent.format( "%s\n", lStrBuffY.toString() );

      FormatterUtils.checkIoException( lFormatterCurrent );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      if( lFormatterCurrent != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        lFormatterCurrent.close();
      }
    }

  }

  private void writeWLSeriesFile( Date pDate, FileObject modelWLFileSerie )
  {
    Formatter lFormatterWL = null;
    try
    {
      List<INodeResult> lListActResults = m_resultsSimpleHandler.getResultsForDate( pDate );
      lFormatterWL = new Formatter( modelWLFileSerie.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      if( lListActResults == null )
      {
        return;
      }
      for( final INodeResult lResultAct : lListActResults )
      {
        if( lResultAct.isWet() )
        {
          lFormatterWL.format( "%.3f\n", lResultAct.getWaterlevel() ); //$NON-NLS-1$
        }
        else
        {
          lFormatterWL.format( "%s\n", ISimulation1D2DConstants.SIM_SWAN_EXCLUSION_NUMBER ); //$NON-NLS-1$
        }
      }
      FormatterUtils.checkIoException( lFormatterWL );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      if( lFormatterWL != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        lFormatterWL.close();
      }
    }
  }

  public static String getActSeriesFileName( String simSwanActSeriesFile, Date pDate )
  {
    if( pDate == null )
    {
      return simSwanActSeriesFile;
    }
    return simSwanActSeriesFile + pDate.getTime();
  }
}
