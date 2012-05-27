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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class TimeseriesMappingBean extends FeatureBean<ITimeseriesMapping>
{
  public TimeseriesMappingBean( final TimeseriesMappingType mappingType )
  {
    super( ITimeseriesMapping.FEATURE_TIMESERIES_MAPPING );

    setProperty( ITimeseriesMapping.PROPERTY_TYPE, mappingType.name() );
  }

  public TimeseriesMappingBean( final ITimeseriesMapping mapping )
  {
    super( mapping );
  }

  public void initFromNaModel( )
  {
    // TODO: read all elements with potential mapping

    // TODO: fill with existing data

    // TODO Auto-generated method stub

  }

  public Feature apply( final CommandableWorkspace workspace, final ITimeseriesMappingCollection timeseriesMappings ) throws Exception
  {
    if( getFeature() == null )
    {
      /* Needs to create new feature */

      final IRelationType relation = (IRelationType) timeseriesMappings.getFeatureType().getProperty( ITimeseriesMappingCollection.MEMBER_TIMESERIES_MAPPING );
      final Map<QName, Object> properties = new HashMap<QName, Object>();

      /* Post the command. */
      final AddFeatureCommand command = new AddFeatureCommand( workspace, ITimeseriesMapping.FEATURE_TIMESERIES_MAPPING, timeseriesMappings, relation, -1, properties, null, -1 );
      workspace.postCommand( command );
    }

    applyChanges();

    return getFeature();
  }
}