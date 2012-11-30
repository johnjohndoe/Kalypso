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

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IMetadata;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.binding.ITarget;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class RainfallCatchmentModel extends Feature_Impl implements IRainfallCatchmentModel
{
  private IFeatureBindingCollection<IMetadata> m_metadataMembers = null;

  private IFeatureBindingCollection<IRainfallGenerator> m_generatorMembers = null;

  public RainfallCatchmentModel( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  @Override
  public String getLogPath( )
  {
    return getProperty( PROPERTY_LOG, String.class );
  }

  @Override
  public URL getLogLocation( ) throws MalformedURLException
  {
    final String logPath = getLogPath();
    if( StringUtils.isBlank( logPath ) )
      return null;

    final URL context = getWorkspace().getContext();
    return new URL( context, logPath );
  }

  @Override
  public ITarget getTarget( )
  {
    return getProperty( MEMBER_TARGET, ITarget.class );
  }

  @Override
  public void setTarget( final ITarget target )
  {
    setProperty( MEMBER_TARGET, target );
  }

  @Override
  public IFeatureBindingCollection<IMetadata> getMetadata( )
  {
    if( m_metadataMembers == null )
      m_metadataMembers = new FeatureBindingCollection<>( this, IMetadata.class, MEMBER_METADATA, true );

    return m_metadataMembers;
  }

  @Override
  public IFeatureBindingCollection<IRainfallGenerator> getGenerators( )
  {
    if( m_generatorMembers == null )
      m_generatorMembers = new FeatureBindingCollection<>( this, IRainfallGenerator.class, MEMBER_GENERATOR, true );

    return m_generatorMembers;
  }
}
