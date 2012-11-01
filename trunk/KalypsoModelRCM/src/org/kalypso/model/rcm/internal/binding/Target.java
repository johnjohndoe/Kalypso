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
package org.kalypso.model.rcm.internal.binding;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IDateRange;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class Target extends Feature_Impl implements ITarget
{
  public Target( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public Feature[] resolveCatchments( ) throws CoreException
  {
    final Object catchmentsObject = getCatchmentObject();
    if( !(catchmentsObject instanceof FeatureList) )
    {
      final String msg = String.format( "Einzugsgebiet-FeaturePfad (catchmentObservationPath) zeigt nicht auf eine FeatureListe: %s", getCatchmentPath() ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, msg ) );
    }

    final FeatureList catchmentFeatures = (FeatureList) catchmentsObject;
    final Feature[] array = FeatureHelper.toArray( catchmentFeatures );
    if( array.length < catchmentFeatures.size() )
      throw new CoreException( new Status( IStatus.WARNING, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("Target_1") ) ); //$NON-NLS-1$

    return array;
  }

  private Object getCatchmentObject( )
  {
    final String catchmentPath = getCatchmentPath();
    final Feature catchmentRootFeature = getResolvedCatchmentFeature();

    final FeaturePath featurePath = new FeaturePath( catchmentPath );
    return featurePath.getFeatureForSegment( catchmentRootFeature.getWorkspace(), catchmentRootFeature, 0 );
  }

  @Override
  public void setCatchmentFeature( final String href )
  {
    setLink( PROPERTY_CATCHMENT_COLLECTION, href );
  }

  @Override
  public Feature getResolvedCatchmentFeature( )
  {
    return FeatureHelper.resolveLink( this, PROPERTY_CATCHMENT_COLLECTION );
  }

  @Override
  public String getCatchmentPath( )
  {
    return getProperty( PROPERTY_FEATURE_PATH, String.class );
  }

  @Override
  public void setCatchmentPath( final String path )
  {
    setProperty( PROPERTY_FEATURE_PATH, path );
  }

  @Override
  public String getObservationPath( )
  {
    return getProperty( PROPERTY_OBSERVATION_PATH, String.class );
  }

  @Override
  public void setObservationPath( final String path )
  {
    setProperty( PROPERTY_OBSERVATION_PATH, path );
  }

  @Override
  public String getFilter( )
  {
    return getProperty( PROPERTY_FILTER, String.class );
  }

  @Override
  public void setFilter( final String filter )
  {
    setProperty( PROPERTY_FILTER, filter );
  }

  @Override
  public DateRange getPeriod( final IStringResolver variables )
  {
    return getProperty( PROPERTY_PERIOD, org.kalypso.model.rcm.internal.binding.DateRange.class ).asDateRange( variables );
  }

  @Override
  public void setPeriod( final DateRange range )
  {
    final IRelationType relation = (IRelationType) getFeatureType().getProperty( PROPERTY_PERIOD );
    final IFeatureType type = GMLSchemaUtilities.getFeatureTypeQuiet( IDateRange.FEATURE_DATE_RANGE );
    final org.kalypso.model.rcm.internal.binding.DateRange periodFeature = (org.kalypso.model.rcm.internal.binding.DateRange) getWorkspace().createFeature( this, relation, type );

    periodFeature.setFrom( range.getFrom() );
    periodFeature.setTo( range.getTo() );

    setProperty( PROPERTY_PERIOD, periodFeature );
  }
}