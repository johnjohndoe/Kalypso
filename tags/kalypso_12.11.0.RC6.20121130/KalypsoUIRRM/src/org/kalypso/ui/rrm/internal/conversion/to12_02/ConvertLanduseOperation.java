/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import com.google.common.base.Charsets;

/**
 * Convert landuse.gml from old namespace to new namespace.d
 * 
 * @author Gernot Belger
 */
public class ConvertLanduseOperation implements ICoreRunnableWithProgress
{
  private final File m_gmlFile;

  public ConvertLanduseOperation( final File gmlFile )
  {
    m_gmlFile = gmlFile;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* load gml file */
      final GMLWorkspace oldWorkspace = GmlSerializer.createGMLWorkspace( m_gmlFile, null );
      final org.kalypso.model.hydrology.binding._11_6.LanduseCollection oldCollection = (org.kalypso.model.hydrology.binding._11_6.LanduseCollection) oldWorkspace.getRootFeature();

      /* create new gml */
      final GMLWorkspace newWorkspace = FeatureFactory.createGMLWorkspace( LanduseCollection.FEATURE_LANDUSE_COLLECTION, null, null );
      final LanduseCollection newCollection = (LanduseCollection) newWorkspace.getRootFeature();

      /* copy data */
      copyData( oldCollection, newCollection );

      /* save new gml */
      GmlSerializer.serializeWorkspace( m_gmlFile, newWorkspace, Charsets.UTF_8.name() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      log.add( IStatus.ERROR, Messages.getString("ConvertLanduseOperation_0"), e ); //$NON-NLS-1$
    }

    return log.asMultiStatus( Messages.getString("ConvertLanduseOperation_1") ); //$NON-NLS-1$
  }

  private void copyData( final org.kalypso.model.hydrology.binding._11_6.LanduseCollection oldCollection, final LanduseCollection newCollection )
  {
    final IFeatureBindingCollection<Landuse> newLanduses = newCollection.getLanduses();
    final IFeatureBindingCollection<org.kalypso.model.hydrology.binding._11_6.Landuse> oldLanduses = oldCollection.getLanduses();

    for( final org.kalypso.model.hydrology.binding._11_6.Landuse oldLanduse : oldLanduses )
    {
      final Landuse newLanduse = newLanduses.addNew( Landuse.FEATURE_LANDUSE );
      newLanduse.setCorrSealing( oldLanduse.getCorrSealing() );
      newLanduse.setName( oldLanduse.getName() );
      newLanduse.setDescription( oldLanduse.getDescription() );
      newLanduse.setGeometry( oldLanduse.getGeometry() );

      final IXLinkedFeature landuseClass = oldLanduse.getLanduse();
      if( landuseClass != null )
        newLanduse.setLanduse( landuseClass.getHref() );
    }
  }
}