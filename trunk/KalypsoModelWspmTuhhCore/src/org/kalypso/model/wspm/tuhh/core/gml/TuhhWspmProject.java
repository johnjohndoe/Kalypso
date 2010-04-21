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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * This is an abstraction layer for tuhh wspm modells. It ensures, that only the right kind of data gets into the model.<br/>
 * It has NO own member variables, everything is backed by the given feature instance.<br/>
 * <br/>
 * FIXME: tricky, this thing is actually not a real abstraction of WspmProject (on gml-level), but is used to mark a
 * project of kind 'tuhh'. We will probably get problems as soon as we introduce a WSPMProject of another kind.
 * 
 * @author Gernot Belger
 */
public class TuhhWspmProject extends WspmProject implements IWspmTuhhConstants
{
  public static final QName QName = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "" );

  public static final QName QNAME_PROP_CALC_MEMBER = new QName( NS_WSPM, "calculationMember" ); //$NON-NLS-1$

  public TuhhWspmProject( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * Adds a new reach to this project.
   * <p>
   * If there is already a water with the given name, use it. If not, generate a new one.
   * </p>
   */
  public TuhhReach createNewReach( final String waterName, final boolean isDirectionUpstreams ) throws GMLSchemaException
  {
    final WspmWaterBody newWater = createWaterBody( waterName, isDirectionUpstreams );
    return createNewReachForWaterBody( newWater );
  }

  /** Not in waterbody, as we create a TuhhWaterBody */
  public static TuhhReach createNewReachForWaterBody( final WspmWaterBody waterBody ) throws GMLSchemaException
  {
    final TuhhReach tuhhReach = (TuhhReach) FeatureHelper.addFeature( waterBody, WspmWaterBody.QNAME_REACH_MEMBER, new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) ); //$NON-NLS-1$

    tuhhReach.setWaterBody( waterBody );

    return tuhhReach;
  }

  /**
   * Adds a new profile (reference) to this project.
   * <p>
   * If there is already a water with the given name, use it. If not, generate a new one.
   * </p>
   */
  public IProfileFeature createNewProfile( final String waterName, final boolean isDirectionUpstreams ) throws GMLSchemaException
  {
    final WspmWaterBody newWater = createWaterBody( waterName, isDirectionUpstreams );
    final IProfileFeature newProfile = newWater.createNewProfile();
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    return newProfile;
  }

  public TuhhCalculation createCalculation( ) throws GMLSchemaException
  {
    return (TuhhCalculation) FeatureHelper.addFeature( this, QNAME_PROP_CALC_MEMBER, TuhhCalculation.QNAME_TUHH_CALC, -1 );
  }

  public TuhhCalculation createReibConstCalculation( ) throws GMLSchemaException
  {
    return (TuhhCalculation) FeatureHelper.addFeature( this, QNAME_PROP_CALC_MEMBER, TuhhCalculation.QNAME_TUHH_CALC_REIB_CONST, -1 );
  }

  public TuhhCalculation[] getCalculations( )
  {
    final GMLWorkspace workspace = getWorkspace();

    final FeatureList calcList = getProperty( QNAME_PROP_CALC_MEMBER, FeatureList.class );
    final List<TuhhCalculation> calcs = new ArrayList<TuhhCalculation>( calcList.size() );
    for( final Object o : calcList )
    {
      final Feature calcFeature;
      if( o instanceof Feature )
        calcFeature = (Feature) o;
      else
        calcFeature = workspace.getFeature( (String) o );

      calcs.add( (TuhhCalculation) calcFeature );
    }

    return calcs.toArray( new TuhhCalculation[calcs.size()] );
  }

  /**
   * Creates a project from scratch. A gml workspace is created internally with the project as root feature.
   */
  public static TuhhWspmProject create( final URL context, final IFeatureProviderFactory factory ) throws InvocationTargetException
  {
    final GMLWorkspace projectWorkspace = FeatureFactory.createGMLWorkspace( QNAME, context, factory );
    final Feature rootFeature = projectWorkspace.getRootFeature();
    return (TuhhWspmProject) rootFeature;
  }
}
