/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.internal.import2dm;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Date;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusPrinter;
import org.kalypso.contribs.eclipse.core.runtime.StatusWithTime;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import com.bce.gis.io.zweidm.IPolygonWithName;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
class SmsDicretisationModelTarget implements ISmsConversionTarget
{
  private final File m_file;

  private DiscretisationModelInserter m_inserter;

  public SmsDicretisationModelTarget( final File outputFile )
  {
    m_file = outputFile;
    try
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IFEDiscretisationModel1d2d.QNAME, null, null );
      final CommandableWorkspace cmdWorkspace = new CommandableWorkspace( workspace );

      final IFEDiscretisationModel1d2d model = (IFEDiscretisationModel1d2d) workspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
      m_inserter = new DiscretisationModelInserter( cmdWorkspace, model );
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public void addElement( final IPolygonWithName surface )
  {
    m_inserter.addElement( surface );
  }

  @Override
  public void finish( )
  {
    final StatusPrinter statusPrinter = new StatusPrinter( 0, new PrintWriter( System.out ) );

    /* Incoming index */
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Building index for new elements...", new Date() ) );
    m_inserter.buildIncomingIndex();
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Index for new elements was built.", new Date() ) );

    /* Incoming validation */
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Validating incoming elements...", new Date() ) );
    final IStatus validateIncomingStatus = m_inserter.validateIncoming();
    statusPrinter.getChildPrinter().print( validateIncomingStatus );

    /* Handle bad incoming */
    final Collection<IPolygonWithName> badElements = m_inserter.getBadIncoming();
    if( badElements.size() > 0 )
    {
      statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Removing bad incoming elements...", new Date() ) );
      m_inserter.removeElements( badElements );
      statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Bad incoming elements removed.", new Date() ) );
    }

    // TODO: check, how new elements intersect with existing
    /* Incoming validation */
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Checking incoming elements against model...", new Date() ) );
    final IStatus validateIncomingModelStatus = m_inserter.validateAgainstModel();
    statusPrinter.getChildPrinter().print( validateIncomingModelStatus );

    /* Handle bad incoming */
    final Collection<IPolygonWithName> badModelElements = m_inserter.getBadAgainstModel();
    if( badModelElements.size() > 0 )
    {
      statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Removing bad incoming elements...", new Date() ) );
      // FIXME: allow for different strategies
      m_inserter.removeElements( badModelElements );
      statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Bad incoming elements removed.", new Date() ) );
    }

    /* Committing changes into model */
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Changing model data...", new Date() ) );
    m_inserter.commitChanges();
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Model changed.", new Date() ) );

    /* Save data */
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Saving changed model...", new Date() ) );
    saveModel();
    statusPrinter.print( new StatusWithTime( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Model saved.", new Date() ) );
  }

  private void saveModel( )
  {
    try
    {
      final IFEDiscretisationModel1d2d model = m_inserter.getModel();
      GmlSerializer.serializeWorkspace( m_file, model.getWorkspace(), "UTF-8" ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
    }
  }
}
