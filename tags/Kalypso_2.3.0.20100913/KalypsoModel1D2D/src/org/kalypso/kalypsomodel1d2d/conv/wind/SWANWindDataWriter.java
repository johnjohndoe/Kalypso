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

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.vfs.FileObject;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.grid.GeoGridException;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author ig
 * 
 */
public class SWANWindDataWriter extends AbstractWindDataWriter
{
  StringBuffer m_strBuffX = null;

  StringBuffer m_strBuffY = null;

  Map<IWindDataProvider, Integer> m_mapFormatterCounter = null;

  Formatter m_formatterWindSeries = null;

  public SWANWindDataWriter( final FileObject outputDirectory, final GM_Envelope gmEnvelopeTarget, final Date[] dates, final List<IWindDataModelSystem> pListSystemsToWrite )
  {
    super( outputDirectory, gmEnvelopeTarget, dates, pListSystemsToWrite );
    m_mapFormatterCounter = new HashMap<IWindDataProvider, Integer>();
  }

  @Override
  protected void writeWindFile( final IWindDataProvider windData )
  {
    //the constant wind will be written for SWAN in to the according control file, so here just quit
    if( m_constantWind ){
      return;
    }
    SWANWindBinaryGeoGridWalker lSWANWalker = null;
    try
    {
      m_strBuffX = new StringBuffer();
      m_strBuffY = new StringBuffer();
      lSWANWalker = new SWANWindBinaryGeoGridWalker( m_gmEnvelope, new StringBuffer[] { m_strBuffX, m_strBuffY }, windData.getGridDescriptor() );
      windData.getDataAsGrid().getWalkingStrategy().walk( windData.getDataAsGrid(), lSWANWalker, null, null );
      if( !hasWritten() )
      {
        m_descriptorWrittenGrid = lSWANWalker.getGridDescriptorVisited();
        setHasWritten( true );
      }
      writeStepIntoFile( windData );
    }
    catch( OperationCanceledException e2 )
    {
      e2.printStackTrace();
    }
    catch( GeoGridException e2 )
    {
      e2.printStackTrace();
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
    }
  }

  private void writeStepIntoFile( final IWindDataProvider windData )
  {
    Formatter lFormatter = null;
    try
    {
      lFormatter = getFormatter( windData );
      writeWindSeriesLine( getFileNameForWindDataProvider( windData ) );
      lFormatter.format( "%s\n%s\n", m_strBuffX.toString(), m_strBuffY.toString() ); //$NON-NLS-1$
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      lFormatter.close();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.wind.AbstractWindDataWriter#afterEnd()
   */
  @Override
  protected void performFinish( )
  {
    try
    {
      if( m_formatterWindSeries != null )
      {
        m_formatterWindSeries.close();
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.wind.AbstractWindDataWriter#beforeStart()
   */
  @Override
  protected void doBeforeStart( )
  {
    try
    {
      m_formatterWindSeries = getFormatter( ISimulation1D2DConstants.SIM_SWAN_WIND_FILE );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  private void writeWindSeriesLine( final String pStrLine )
  {
    try
    {
      m_formatterWindSeries.format( "%s\n", pStrLine ); //$NON-NLS-1$
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.wind.AbstractWindDataWriter#getFormatter(org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider)
   */
  @Override
  protected Formatter getFormatter( final Object pObject ) throws IOException
  {
    Formatter lFormatter = null;
    if( pObject != null && pObject instanceof IWindDataProvider )
    {
      IWindDataProvider windData = (IWindDataProvider) pObject;
      {
        final FileObject lFileWindData = m_fileOutputDir.resolveFile( getFileNameForWindDataProvider( windData ) );
        lFormatter = new Formatter( lFileWindData.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      }
    }
    else if( pObject != null && pObject instanceof String )
    {
      FileObject lFileWindDataAdditional = m_fileOutputDir.resolveFile( (String) pObject + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT );
      lFormatter = new Formatter( lFileWindDataAdditional.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
    }
    else
    {
      lFormatter = null;
    }

    return lFormatter;
  }

  private String getFileNameForWindDataProvider( IWindDataProvider windData )
  {
    return ISimulation1D2DConstants.SIM_SWAN_WIND_FILE + ISimulation1D2DConstants.SIM_SWAN_TIME_SUFFIX + windData.getDateStep().getTime() + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
  }
}
