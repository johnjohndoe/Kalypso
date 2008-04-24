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
package org.kalypso.model.wspm.schema.gml;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.model.feature.GmlWorkspaceListener;

/**
 * TODO: does not work at the moment, because we dont get enough information from the modell events.
 * 
 * @author Gernot Belger
 */
public class ProfilLineCacheListener extends GmlWorkspaceListener implements IGmlWorkspaceListener, FeatureVisitor
{
  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#init(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void init( final GMLWorkspace workspace )
  {
    // workspace.accept( this, workspace.getRootFeature(), DEPTH_INFINITE );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // if( modellEvent instanceof FeaturesChangedModellEvent )
    // {
    // final FeaturesChangedModellEvent fcme = (FeaturesChangedModellEvent) modellEvent;
    // final List features = fcme.getFeatures();
    // for( final Object object : features )
    // {
    // final Feature f = (Feature) object;
    //
    // }
    // }
    // else if( modellEvent instanceof FeatureStructureChangeModellEvent )
    // {
    // final FeatureStructureChangeModellEvent fscme = (FeatureStructureChangeModellEvent) modellEvent;
    // }
    //
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    // recalcFeature( f );

    return true;
  }

  // private void recalcFeature( final Feature f )
  // {
  // if( !GMLSchemaUtilities.substitutes( f.getFeatureType(), WspmProfile.QNAME_PROFILE ) )
  // return;
  //
  // final WspmProfile profile = new WspmProfile( f );
  // }
}
