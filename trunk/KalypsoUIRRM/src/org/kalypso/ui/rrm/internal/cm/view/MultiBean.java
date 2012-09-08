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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class MultiBean extends FeatureBean<IMultiGenerator>
{
  /**
   * The checked sub generators.
   */
  private ILinearSumGenerator[] m_subGenerators;

  /**
   * The status.
   */
  private IStatus m_status;

  /**
   * Constructs a complete empty multi bean.
   */
  public MultiBean( )
  {
    super( IMultiGenerator.FEATURE_MULTI_GENERATOR );

    m_subGenerators = new ILinearSumGenerator[] {};
  }

  /**
   * Constructs a multi bean initialized with the values from the multi generator.
   *
   * @param generator
   *          The multi generator.
   */
  public MultiBean( final IMultiGenerator generator )
  {
    super( generator );

    m_subGenerators = initSubGenerators();
  }

  /**
   * This function returns the checked sub generators.
   *
   * @return The checked sub generators.
   */
  public ILinearSumGenerator[] getSubGenerators( )
  {
    return m_subGenerators;
  }

  /**
   * This function sets the checked sub generators.
   *
   * @param subGenerators
   *          The checked sub generators.
   */
  public void setSubGenerators( final ILinearSumGenerator[] subGenerators )
  {
    m_subGenerators = subGenerators;
  }

  /**
   * This function updates the status.
   */
  public void updateStatus( )
  {
    m_status = CatchmentModelHelper.validateMultiBean( this );
  }

  /**
   * This function returns the status.
   *
   * @return The status.
   */
  public IStatus getStatus( )
  {
    return m_status;
  }

  /**
   * This function applies the changes of this multi generator. It will create or update the multi generator feature.
   *
   * @param workspace
   *          The workspace.
   * @return The new multi generator.
   */
  public IMultiGenerator apply( final CommandableWorkspace workspace ) throws Exception
  {
    /* Get the feature. */
    final IMultiGenerator feature = getFeature();
    if( feature == null )
    {
      /* The multi generator feature does not exist. */
      final Map<QName, Object> properties = new HashMap<>( getProperties() );
      final ICatchmentModel collection = (ICatchmentModel) workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchmentModel.MEMBER_CATCHMENT_GENERATOR );
      final QName type = getFeatureType().getQName();

      /* Create the add feature command. */
      final AddFeatureCommand command = new AddFeatureCommand( workspace, type, collection, parentRelation, -1, properties, null, -1 );

      /* Post the command. */
      workspace.postCommand( command );

      /* Get the new feature. */
      final IMultiGenerator newFeature = (IMultiGenerator) command.getNewFeature();

      /* Apply the sub generators. */
      applySubGenerators( workspace, newFeature );

      /* Set the last modified timestamp. */
      newFeature.setLastModified( new Date() );

      return newFeature;
    }

    /* Apply the changes. */
    final ICommand command = applyChanges();

    /* Post the command. */
    workspace.postCommand( command );

    /* Remove the sub generators. */
    removeSubGenerators( workspace, feature );

    /* Apply the sub generators. */
    applySubGenerators( workspace, feature );

    /* Set the last modified timestamp. */
    feature.setLastModified( new Date() );

    return feature;
  }

  private ILinearSumGenerator[] initSubGenerators( )
  {
    final List<ILinearSumGenerator> results = new ArrayList<>();

    final IMultiGenerator generator = getFeature();
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators = generator.getSubGenerators();
    for( final IRainfallGenerator subGenerator : subGenerators )
      results.add( (ILinearSumGenerator) subGenerator );

    return results.toArray( new ILinearSumGenerator[] {} );
  }

  private void removeSubGenerators( final CommandableWorkspace workspace, final IMultiGenerator multiGenerator ) throws Exception
  {
    /* Get all sub generators. */
    final FeatureList subGenerators = (FeatureList) multiGenerator.getProperty( IMultiGenerator.MEMBER_SUB_GENERATOR );
    subGenerators.clear();

    /* Post the command. */
    workspace.postCommand( new EmptyCommand( Messages.getString("MultiBean_0"), true ) ); //$NON-NLS-1$
  }

  private void applySubGenerators( final CommandableWorkspace workspace, final IMultiGenerator multiGenerator ) throws Exception
  {
    /* Nothing to do. */
    if( m_subGenerators == null || m_subGenerators.length == 0 )
      return;

    /* Get the list of the sub generators. */
    final FeatureList subGenerators = (FeatureList) multiGenerator.getProperty( IMultiGenerator.MEMBER_SUB_GENERATOR );

    /* Create the add commands. */
    for( final ILinearSumGenerator subGenerator : m_subGenerators )
      subGenerators.addLink( subGenerator );

    /* Post the command. */
    workspace.postCommand( new EmptyCommand( Messages.getString("MultiBean_1"), true ) ); //$NON-NLS-1$
  }
}