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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * Reads the contents of 'HOW_QWehr_HUW.txt' into the qIntervallResults.
 * 
 * @author Gernot Belger
 */
public class BuildingPolygonReader
{
  private final LogHelper m_log;

  private final QIntervalIndex m_intervalIndex;

  public BuildingPolygonReader( final QIntervalIndex intervalIndex, final LogHelper log )
  {
    m_intervalIndex = intervalIndex;
    m_log = log;
  }

  public void read( final File buildingFile ) throws IOException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( buildingFile ) );

      /* Ingore first line */
      if( reader.ready() )
        reader.readLine();

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        try
        {
          readBuildingLine( line.trim(), buildingFile );
        }
        catch( final NumberFormatException nfe )
        {
          /* A good line but bad content. Give user a hint that something might be wrong. */
          m_log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.25" ), buildingFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() ); //$NON-NLS-1$
        }
        catch( final Throwable e )
        {
          // should never happen
          m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.25" ), buildingFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() ); //$NON-NLS-1$
        }

      }
      reader.close();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private void readBuildingLine( final String line, final File buildingFile ) throws Exception
  {
    final String[] tokens = line.split( " +" ); //$NON-NLS-1$
    if( tokens.length < 6 )
      return;

    /* Determine if this is a good line: good lines are lines whose first token is a number */
    final BigDecimal station;
    try
    {
      station = new BigDecimal( tokens[0] );
    }
    catch( final NumberFormatException nfe )
    {
      /* Just ignore this line */
      return;
    }

    final BigDecimal qOW = new BigDecimal( tokens[1] );
    /* final BigDecimal qWehr = new BigDecimal( tokens[2] ); */
    final BigDecimal hOW = new BigDecimal( tokens[3] );
    final BigDecimal hUW = new BigDecimal( tokens[4] );
    /* final String ueArt = tokens[5]; */

    final QIntervallResult qresult = m_intervalIndex.addOrGet( station );

    /* Add comment */
    qresult.setDescription( qresult.getDescription() + Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.27" ) + buildingFile.getName() ); //$NON-NLS-1$

    /* Add values to the weir observation */
    final IObservation<TupleResult> buildingObs = qresult.getBuildingObservation( true );

    final String buildingId = qresult.getBuildingId();
    if( buildingId != null )
    {
      /* Set the phenomenon of the building as phenomenon for the observation */
      final IPhenomenon buildingPhenomenon = new Phenomenon( buildingId, "", "" ); //$NON-NLS-1$ //$NON-NLS-2$
      buildingObs.setPhenomenon( buildingPhenomenon );
    }

    final TupleResult result = buildingObs.getResult();
    final IComponent[] components = result.getComponents();
    final IComponent compHOW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    final IComponent compHUW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    final IComponent compRunoff = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );

    // put extra result into observation
    final IRecord newRecord = result.createRecord();
    newRecord.setValue( result.indexOfComponent( compRunoff ), qOW );
    newRecord.setValue( result.indexOfComponent( compHOW ), hOW );
    newRecord.setValue( result.indexOfComponent( compHUW ), hUW );
    result.add( newRecord );

    qresult.setBuildingObservation( buildingObs );
  }
}
