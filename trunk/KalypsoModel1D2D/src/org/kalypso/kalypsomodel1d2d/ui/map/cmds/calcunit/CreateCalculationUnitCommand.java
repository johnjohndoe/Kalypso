/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.Map;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

final class CreateCalculationUnitCommand extends AddFeatureCommand
{
  private final IControlModel1D2DCollection m_parentFeature;

  private final ICalculationUnit m_calculationUnit;

  CreateCalculationUnitCommand( final GMLWorkspace workspace, final IFeatureType type, final Feature parentFeature, final IRelationType propertyName, final int pos, final Map<IPropertyType, Object> properties, final IFeatureSelectionManager selectionManager, final int depth, final IControlModel1D2DCollection parentFeature2, final ICalculationUnit calculationUnit )
  {
    super( workspace, type, parentFeature, propertyName, pos, properties, selectionManager, depth );

    m_parentFeature = parentFeature2;
    m_calculationUnit = calculationUnit;
  }

  @Override
  public void process( ) throws Exception
  {
    super.process();

    final Feature newControlFeature = getNewFeature();
    final IControlModel1D2D newControlModel = (IControlModel1D2D)newControlFeature.getAdapter( IControlModel1D2D.class );

    // FIXME: m_calculationUnit is always null at this point; shouldn't it be newControlModel?
    newControlModel.setCalculationUnit( m_calculationUnit );

    m_parentFeature.setActiveControlModel( newControlModel );

    final Feature obsFeature = (Feature)newControlFeature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );

    /* Create an observation from it. */
    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
    final TupleResult result = obs.getResult();
    /* If not yet initialized, create components and write obs back to feature. */
    if( result.getComponents().length == 0 )
    {
      obs.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.3" ) ); //$NON-NLS-1$

      // TODO put this inside c1d2d:TimestepsObservation
      /**
       * <om:observedProperty xmlns:om="http://www.opengis.net/om"
       * xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
       * IPhenomenon phenomenon = new DictionaryPhenomenon(
       * "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", "", "");
       * obs.setPhenomenon( phenomenon );
       */

      final String[] componentUrns = new String[] { Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME,
          Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR };
      final IComponent[] components = new IComponent[componentUrns.length];

      for( int i = 0; i < components.length; i++ )
        components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

      for( final IComponent component : components )
        result.addComponent( component );

      result.setSortComponents( new IComponent[] { components[1] } );
      result.setOrdinalNumberComponent( components[0] );

      ObservationFeatureFactory.toFeature( obs, obsFeature );
    }
  }
}