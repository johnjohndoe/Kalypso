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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class LinearSumBean extends FeatureBean<ILinearSumGenerator>
{
  private CatchmentBean[] m_catchments;

  public LinearSumBean( )
  {
    super( ILinearSumGenerator.FEATURE_LINEAR_SUM_GENERATOR );

    m_catchments = new CatchmentBean[] {};
  }

  public LinearSumBean( final ILinearSumGenerator generator )
  {
    super( generator );

    m_catchments = initCatchments();
  }

  public String getLabel( )
  {
    return (String) getProperty( Feature.QN_DESCRIPTION );
  }

  public CatchmentBean[] getCatchments( )
  {
    return m_catchments;
  }

  private CatchmentBean[] initCatchments( )
  {
    final List<CatchmentBean> results = new ArrayList<CatchmentBean>();
    final ILinearSumGenerator generator = getFeature();
    final ICatchment[] catchments = generator.getCatchments();
    for( final ICatchment catchment : catchments )
      results.add( new CatchmentBean( catchment ) );

    return results.toArray( new CatchmentBean[] {} );
  }

  public void setCatchments( final CatchmentBean[] catchments )
  {
    m_catchments = catchments;
  }

  /**
   * This function applies the changes of this linear sum generator. It will create or update the linear sum generator
   * feature. If the linear sum generator feature is existing, its (probaply) existing catchment features will be
   * deleted. So always new ones will be generated.
   * 
   * @param workspace
   *          The workspace.
   * @param parameterType
   *          The selected parameter type.
   */
  public ILinearSumGenerator apply( final CommandableWorkspace workspace, final String parameterType ) throws Exception
  {
    /* Get the feature. */
    final ILinearSumGenerator feature = getFeature();
    if( feature == null )
    {
      /* The linear sum generator feature does not exist. */
      final Map<QName, Object> properties = new HashMap<QName, Object>( getProperties() );
      properties.put( ILinearSumGenerator.PROPERTY_AREA_NAME, "gml:name" );
      properties.put( ILinearSumGenerator.PROPERTY_AREA_DESCRIPTION, "gml:description" );
      final ICatchmentModel collection = (ICatchmentModel) workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchmentModel.MEMBER_CATCHMENT_GENERATOR );
      final QName type = getFeatureType().getQName();

      /* Create the add feature command. */
      final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

      /* Post the command. */
      workspace.postCommand( command );

      /* Apply also all contained catchments. */
      for( final CatchmentBean catchment : m_catchments )
        catchment.apply( workspace, command.getNewFeature(), parameterType );

      return (ILinearSumGenerator) command.getNewFeature();
    }

    /* Apply the changes. */
    final ICommand command = applyChanges();

    /* Post the command. */
    workspace.postCommand( command );

    /* The delete commands. */
    final CompositeCommand deleteCommands = new CompositeCommand( "Updating catchments..." );

    /* Get all catchments. */
    final ICatchment[] catchments = feature.getCatchments();
    for( final ICatchment catchment : catchments )
      deleteCommands.addCommand( new DeleteFeatureCommand( catchment ) );

    /* Post the command. */
    workspace.postCommand( deleteCommands );

    /* Apply also all contained catchments. */
    for( final CatchmentBean catchment : m_catchments )
      catchment.apply( workspace, feature, parameterType );

    return feature;
  }

  public boolean hasDescription( )
  {
    final Object property = getProperty( ILinearSumGenerator.QN_DESCRIPTION );
    if( property == null )
      return false;

    return true;
  }
}