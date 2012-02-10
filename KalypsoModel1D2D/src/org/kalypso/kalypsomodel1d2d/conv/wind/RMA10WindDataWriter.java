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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;

import org.apache.commons.vfs.FileObject;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.grid.GeoGridException;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author ig
 * 
 */
public class RMA10WindDataWriter extends AbstractWindDataWriter
{
  private List<GM_Position> m_listRMA10Nodes = null;

  private Formatter m_formatter = null;

  public RMA10WindDataWriter( final FileObject outputDirectory, final GM_Envelope gmEnvelopeTarget, final Date[] dates, final List<IWindDataModelSystem> pListSystemsToWrite )
  {
    super( outputDirectory, gmEnvelopeTarget, dates, pListSystemsToWrite );
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void writeWindFile( final IWindDataProvider windData ) throws IOException
  {
    m_formatter = getFormatter( windData );
    writeStepTimeLine( m_formatter, windData.getDateStep() );
    RMA10WindBinaryGeoGridWalker lRMA10Walker = null;
    try
    {
      lRMA10Walker = new RMA10WindBinaryGeoGridWalker( m_gmEnvelope, m_formatter, windData.getGridDescriptor() );
      Object lResulWalk = windData.getDataAsGrid().getWalkingStrategy().walk( windData.getDataAsGrid(), lRMA10Walker, null, null );
      if( !hasWritten() )
      {
        m_descriptorWrittenGrid = lRMA10Walker.getGridDescriptorVisited();
        m_listRMA10Nodes = new ArrayList<GM_Position>();
        m_listRMA10Nodes.addAll( (List<GM_Position>) lResulWalk );
        setHasWritten( true );
      }
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

  private void writeStepTimeLine( final Formatter formatterWindData, final Date actDate )
  {
    Calendar lCal = Calendar.getInstance();
    lCal.setTime( actDate );
    double lDoubleTime = lCal.get( Calendar.HOUR_OF_DAY );
    lDoubleTime += ((double) lCal.get( Calendar.MINUTE )) / 100;
    formatterWindData.format( "DY           %2.2f %7d %7d\n", lDoubleTime, lCal.get( Calendar.DAY_OF_YEAR ), lCal.get( Calendar.YEAR ) ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.wind.AbstractWindDataWriter#afterEnd()
   */
  @Override
  protected void performFinish( )
  {
    Formatter lFormatter = null;
    try
    {
      lFormatter = getFormatter( null );
      lFormatter.format( "ENDDATA\n" ); //$NON-NLS-1$
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    finally 
    {
      lFormatter.close();
    }
    writeRMA10WindNodes();
  }

  private void writeRMA10WindNodes( )
  {
    Formatter lFormatter = null;
    int lIntSecondCorner = m_descriptorWrittenGrid == null? 0: m_descriptorWrittenGrid.getNumColumns();
    int lIntThirdCorner = m_descriptorWrittenGrid == null? 0: m_descriptorWrittenGrid.getNumColumns() * (m_descriptorWrittenGrid.getNumRows() - 1) + 1;
    int lIntSize = m_listRMA10Nodes == null ? 0 : m_listRMA10Nodes.size();
    int lIntCount = 1;
    try
    {
      lFormatter = getFormatter( ISimulation1D2DConstants.WIND_RMA10_COORDS_File );
      lFormatter.format( "NVT    %d\n", lIntSize ); //$NON-NLS-1$
      if( hasWritten() )
      {
        for( final GM_Position lPositionNode : m_listRMA10Nodes )
        {
          lFormatter.format( "CRD            %d %5.5f %5.5f\n", lIntCount++, lPositionNode.getX(), lPositionNode.getY() ); //$NON-NLS-1$
        }
        lFormatter.format( "OUT            %d      %d      %d      %d\n", 1, lIntSecondCorner, lIntThirdCorner, lIntSize );//$NON-NLS-1$
      } 

      lFormatter.format( "ENDDATA\n" );//$NON-NLS-1$
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      if( lFormatter != null )
      {
        lFormatter.close();
      }
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
      Formatter lFormatter = getFormatter( null );
      lFormatter.format( "TT      Winddata for RMA10\n" ); //$NON-NLS-1$
    }
    catch( IOException e )
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
    if( pObject == null || pObject instanceof IWindDataProvider )
    {
      if( m_formatter == null )
      {
        final FileObject lFileWindData = m_fileOutputDir.resolveFile( ISimulation1D2DConstants.WIND_RMA10_File );
        m_formatter = new Formatter( lFileWindData.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
      }
    }
    else if( pObject != null && pObject instanceof String )
    {
      FileObject lFileWindDataAdditional = m_fileOutputDir.resolveFile( (String) pObject );
      m_formatter = new Formatter( lFileWindDataAdditional.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
    }
    else
    {
      return null;
    }

    return m_formatter;
  }

}
