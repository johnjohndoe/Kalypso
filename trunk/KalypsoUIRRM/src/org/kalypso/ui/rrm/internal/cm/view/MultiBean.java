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

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author Holger Albert
 */
public class MultiBean extends FeatureBean<IMultiGenerator>
{
  /**
   * The constructor.
   */
  public MultiBean( )
  {
    super( IMultiGenerator.FEATURE_MULTI_GENERATOR );
  }

  /**
   * The constructor.
   * 
   * @param generator
   *          The multi generator.
   */
  public MultiBean( final IMultiGenerator generator )
  {
    super( generator );
  }

  /**
   * This function returns the label.
   * 
   * @return The label.
   */
  public String getLabel( )
  {
    return (String) getProperty( Feature.QN_DESCRIPTION );
  }

  /**
   * This function returns true, if there is an description.
   * 
   * @return True, if there is an description.
   */
  public boolean hasDescription( )
  {
    final Object property = getProperty( IMultiGenerator.QN_DESCRIPTION );
    if( property == null )
      return false;

    return true;
  }

  /**
   * This function applies the changes of this multi generator. It will create or update the multi generator feature.
   * 
   * @param workspace
   *          The workspace.
   * @param parameterType
   *          The selected parameter type.
   * @return The new multi generator.
   */
  public IMultiGenerator apply( final CommandableWorkspace workspace, final String parameterType ) throws Exception
  {
    /* Get the feature. */
    final IMultiGenerator feature = getFeature();
    if( feature == null )
    {
      /* The multi generator feature does not exist. */
      final Map<QName, Object> properties = new HashMap<QName, Object>( getProperties() );
      final ICatchmentModel collection = (ICatchmentModel) workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchmentModel.MEMBER_CATCHMENT_GENERATOR );
      final QName type = getFeatureType().getQName();

      /* Create the add feature command. */
      final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

      /* Post the command. */
      workspace.postCommand( command );

      /* Apply the sub generators. */
      applySubGenerators();

      return (IMultiGenerator) command.getNewFeature();
    }

    /* Apply the changes. */
    final ICommand command = applyChanges();

    /* Post the command. */
    workspace.postCommand( command );

    /* The delete commands. */
    final CompositeCommand deleteCommands = new CompositeCommand( "Updating sub generators..." );

    /* Get all. */
    // TODO
    final FeatureList generators = (FeatureList) feature.getProperty( IMultiGenerator.MEMBER_SUB_GENERATOR );
    for( final Object generator : generators )
      deleteCommands.addCommand( new DeleteFeatureCommand( (Feature) generator ) );

    /* Post the command. */
    workspace.postCommand( deleteCommands );

    /* Apply the sub generators. */
    applySubGenerators();

    return feature;
  }

  /**
   * This function creates the links for the sub generators.
   */
  private void applySubGenerators( )
  {
    // TODO
  }
}