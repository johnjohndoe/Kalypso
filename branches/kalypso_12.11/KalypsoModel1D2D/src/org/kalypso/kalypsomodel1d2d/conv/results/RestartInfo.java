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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Dejan Antanaskovic
 */
public class RestartInfo extends Feature_Impl implements IRestartInfo
{
  public RestartInfo( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IPath getRestartFilePath( )
  {
    final String path = (String)getProperty( IRestartInfo.QNAME_PROP_RESULT_FILE_PATH );
    if( path == null )
      return null;

    return Path.fromPortableString( path );
  }

  @Override
  public void setRestartFilePath( final String filePath )
  {
    setProperty( IRestartInfo.QNAME_PROP_RESULT_FILE_PATH, filePath );
  }

  @Override
  public URL resolveRestartLocation( final URL currentScenario ) throws MalformedURLException
  {
    final String path = (String)getProperty( IRestartInfo.QNAME_PROP_RESULT_FILE_PATH );
    if( StringUtils.isBlank( path ) )
      return null;

    return UrlResolverSingleton.resolveUrl( currentScenario, path );
  }
}