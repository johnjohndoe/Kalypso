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
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Thomas Jung
 */
public class RunoffEvent extends AbstractFeatureBinder implements IRunoffEvent
{
  private final FeatureWrapperCollection<ITinReference> m_tinReferences;

  public RunoffEvent( final Feature featureToBind )
  {
    super( featureToBind, IRunoffEvent.QNAME );

    m_tinReferences = new FeatureWrapperCollection<ITinReference>( featureToBind, ITinReference.class, QNAME_PROP_TIN_MEMBER );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getResultCoverages()
   */
  @Override
  public ICoverageCollection getResultCoverages( )
  {
    final Feature coveragesFeature = (Feature) getFeature().getProperty( QNAME_PROP_RESULT_COVERAGES );
    if( coveragesFeature == null )
      return internal_createResultCoverages();

    return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
  }

  /**
   * Creates a new result collection and returns it.<br/>
   * If the result collection already exists, the existing one will be returned.
   * 
   * @see org.kalypso.model.flood.binding.IRunoffEvent#createResultCoverages()
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
    final IRelationType relationType = (IRelationType) getFeature().getFeatureType().getProperty( QNAME_PROP_RESULT_COVERAGES );
    final GMLWorkspace workspace = getFeature().getWorkspace();
    final Feature newFeature = workspace.createFeature( getFeature(), relationType, relationType.getTargetFeatureType() );
    final ICoverageCollection newCollection = (ICoverageCollection) newFeature.getAdapter( ICoverageCollection.class );
    setResultCoverages( newCollection );
    return newCollection;
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getTins()
   */
  @Override
  public IFeatureWrapperCollection<ITinReference> getTins( )
  {
    return m_tinReferences;
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getDataPath()
   */
  @Override
  public IPath getDataPath( )
  {
    final String path = (String) getFeature().getProperty( QNAME_PROP_DATAPATH );
    if( path == null )
      return null;

    return Path.fromPortableString( path );
  }

  @Override
  public void setDataPath( final IPath path )
  {
    getFeature().setProperty( QNAME_PROP_DATAPATH, path.toPortableString() );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#setResultCoverages(org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection)
   */
  @Override
  public void setResultCoverages( final ICoverageCollection collection )
  {
    getFeature().setProperty( QNAME_PROP_RESULT_COVERAGES, collection );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#isMarkedForProcess()
   */
  @Override
  public boolean isMarkedForProcessing( )
  {
    final Boolean value = (Boolean) getFeature().getProperty( QNAME_PROP_MARKEDFORPROCESSING );
    return value == null ? false : value.booleanValue();
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#setMarkedForProcess(boolean)
   */
  @Override
  public void setMarkedForProcessing( final boolean value )
  {
    getFeature().setProperty( QNAME_PROP_MARKEDFORPROCESSING, value );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getReturnPeriod()
   */
  @Override
  public Integer getReturnPeriod( )
  {
    return (Integer) getFeature().getProperty( QNAME_PROP_RETURN_PERIOD );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#setReturnPeriod(java.lang.Integer)
   */
  @Override
  public void setReturnPeriod( final Integer value )
  {
    getFeature().setProperty( QNAME_PROP_RETURN_PERIOD, value );
  }
}
