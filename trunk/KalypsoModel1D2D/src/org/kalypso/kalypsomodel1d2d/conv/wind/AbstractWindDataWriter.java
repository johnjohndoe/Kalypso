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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Formatter;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * @author ig
 */
abstract public class AbstractWindDataWriter implements IWindDataWriter
{
  protected IWindModel m_windDataModel = null;

  protected List<IWindDataModelSystem> m_listWindSystemsToWrite = null;

  protected File m_fileOutputDir = null;

  protected RectifiedGridDomain m_descriptorWrittenGrid = null;

  protected boolean m_hasWritten = false;

  protected boolean m_constantWind = false;

  protected List<Date> m_listWrittenDates = null;

  protected GM_Envelope m_gmEnvelope;

  protected Date[] m_dates;

  protected int m_intScale = 1;

  protected AbstractWindDataWriter( final File pOutputDirectory, final GM_Envelope pGmEnvelopeTarget, final Date[] pDates, final List<IWindDataModelSystem> pListSystemsToWrite )
  {
    m_listWrittenDates = new ArrayList<>();
    m_gmEnvelope = pGmEnvelopeTarget;
    m_dates = pDates;
    m_fileOutputDir = pOutputDirectory;
    m_listWindSystemsToWrite = pListSystemsToWrite;
  }

  @Override
  public boolean write( final boolean pBoolConstantWind ) throws IOException
  {
    m_constantWind = pBoolConstantWind;
    if( m_windDataModel == null && !m_constantWind )
    {
      return false;
    }
    final List<IWindDataModelSystem> lListSystems = m_windDataModel.getWindDataModelSystems();
    Collections.sort( lListSystems, new Comparator<IWindDataModelSystem>()
    {
      @Override
      public int compare( final IWindDataModelSystem o1, final IWindDataModelSystem o2 )
      {
        try
        {
          return o1.getOrder() - o2.getOrder();
        }
        catch( final Exception e )
        {
          return -1;
        }
      }
    } );
    doBeforeStart();
    for( final IWindDataModelSystem lWindSystem : lListSystems )
    // for( final IWindDataModelSystem lWindSystem : m_windDataModel.getWindDataModelSystems() )
    {
      if( !m_listWindSystemsToWrite.contains( lWindSystem ) )
      {
        continue;
      }

      for( final Object lWindDataObject : lWindSystem.getWindDataModels() )
      {
        final IWindDataProvider lWindData = (IWindDataProvider)lWindDataObject;
        if( lWindData.getDateStep().getTime() >= m_dates[0].getTime() && lWindData.getDateStep().getTime() <= m_dates[m_dates.length - 1].getTime() )
        {
          writeWindFile( lWindData );
          m_listWrittenDates.add( lWindData.getDateStep() );
          lWindData.getDataAsGrid().dispose();
        }
      }
      // the writing of all overlapping wind systems is not implemented yet, so we write at the moment only the first
      // one out
      break;
    }
    performFinish();

    return true;
  }

  /**
   * has to be implemented with actual wind data step handling final
   */
  abstract protected void writeWindFile( final IWindDataProvider windData ) throws IOException;

  /**
   * has to be implemented for the case of special handling of data before writing it into target file
   */
  abstract protected void doBeforeStart( );

  /**
   * has to be implemented for the case of special handling of data after writing it into target file i.e. for writing
   * additional files with additional information
   */
  abstract protected void performFinish( );

  /**
   * has to be implemented for getting the formatter for actual wind data step or for file name
   */
  abstract protected Formatter getFormatter( final Object pObject ) throws IOException;

  @Override
  public List<Date> getListWritenDates( )
  {
    return m_listWrittenDates;
  }

  @Override
  public RectifiedGridDomain getWrittenGrid( )
  {
    return m_descriptorWrittenGrid;
  }

  @Override
  public void setWindDataModel( final IWindModel pWindModel ) throws IllegalArgumentException
  {
    m_windDataModel = pWindModel;
  }

  protected final boolean hasWritten( )
  {
    return m_hasWritten;
  }

  protected final void setHasWritten( final boolean pHasWritten )
  {
    m_hasWritten = pHasWritten;
  }
}