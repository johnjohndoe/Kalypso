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
package org.kalypso.model.flood.binding;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Thomas Jung
 */
public class RunoffEvent extends Feature_Impl implements IRunoffEvent
{
  private final FeatureBindingCollection<ITinReference> m_tinReferences;

  public RunoffEvent( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
    m_tinReferences = new FeatureBindingCollection<>( this, ITinReference.class, QNAME_PROP_TIN_MEMBER );
  }

  @Override
  public ICoverageCollection getResultCoverages( )
  {
    final Feature coveragesFeature = (Feature) getProperty( QNAME_PROP_RESULT_COVERAGES );
    if( coveragesFeature == null )
      return internal_createResultCoverages();

    return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
  }

  /**
   * Creates a new result collection and returns it.<br/>
   * If the result collection already exists, the existing one will be returned.
   */
  @Override
  public ICoverageCollection createResultCoverages( )
  {
    final ICoverageCollection existingCoverages = getResultCoverages();
    if( existingCoverages != null )
      return existingCoverages;
    return internal_createResultCoverages();
  }

  private ICoverageCollection internal_createResultCoverages( )
  {
    final IRelationType relationType = (IRelationType) getFeatureType().getProperty( QNAME_PROP_RESULT_COVERAGES );
    final GMLWorkspace workspace = getWorkspace();
    final Feature newFeature = workspace.createFeature( this, relationType, relationType.getTargetFeatureType() );
    final ICoverageCollection newCollection = (ICoverageCollection) newFeature.getAdapter( ICoverageCollection.class );
    setResultCoverages( newCollection );
    return newCollection;
  }

  @Override
  public IFeatureBindingCollection<ITinReference> getTins( )
  {
    return m_tinReferences;
  }

  @Override
  public IPath getDataPath( )
  {
    final String path = (String) getProperty( QNAME_PROP_DATAPATH );
    if( path == null )
      return null;

    return Path.fromPortableString( path );
  }

  @Override
  public void setDataPath( final IPath path )
  {
    setProperty( QNAME_PROP_DATAPATH, path.toPortableString() );
  }

  @Override
  public void setResultCoverages( final ICoverageCollection collection )
  {
    setProperty( QNAME_PROP_RESULT_COVERAGES, collection );
  }

  @Override
  public boolean isMarkedForProcessing( )
  {
    final Boolean value = (Boolean) getProperty( QNAME_PROP_MARKEDFORPROCESSING );
    return value == null ? false : value.booleanValue();
  }

  @Override
  public void setMarkedForProcessing( final boolean value )
  {
    setProperty( QNAME_PROP_MARKEDFORPROCESSING, value );
  }

  @Override
  public Integer getReturnPeriod( )
  {
    return (Integer) getProperty( QNAME_PROP_RETURN_PERIOD );
  }

  @Override
  public void setReturnPeriod( final Integer value )
  {
    setProperty( QNAME_PROP_RETURN_PERIOD, value );
  }
}