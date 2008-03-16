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
package org.kalypso.model.wspm.sobek.core.model;

import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author kuch
 */
public class CrossSectionNode extends AbstractNode implements ICrossSectionNode
{
  public CrossSectionNode( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    FeatureUtils.deleteFeature( getModel().getWorkspace(), getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getLinkToBranch()
   */
  public IBranch getLinkToBranch( )
  {
    final Object objBranch = getFeature().getProperty( ISobekConstants.QN_LN_LINKS_TO_BRANCH );
    final Feature f;
    if( objBranch instanceof Feature )
      // this branch should never be reached according to the schema file
      f = (Feature) objBranch;
    else
      f = getFeature().getWorkspace().getFeature( (String) objBranch );

    final IBranch result = new Branch( getModel(), f );

    if( result == null )
      return null;
    return result;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getCrossSectionData()
   */
  public IProfil getProfile( )
  {
    final WspmProfile wspmProfile = getWspmProfile();
    final IProfil profil = wspmProfile.getProfil();

    return profil;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return TYPE.eCrossSectionNode;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getWspmProfile()
   */
  public WspmProfile getWspmProfile( )
  {
    final Feature f = getLinkedProfile();

    return new WspmProfile( f );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getLinkedProfile()
   */
  public Feature getLinkedProfile( )
  {
    Feature f = null;

    final Feature feature = getFeature();
    final Object objCsData = feature.getProperty( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE );
    if( objCsData instanceof XLinkedFeature_Impl )
    {
      final XLinkedFeature_Impl lnk = (XLinkedFeature_Impl) objCsData;
      f = lnk.getFeature();
    }
    else if( objCsData instanceof Feature )
      // this branch should never be reached according to the schema file
      f = (Feature) objCsData;
    else
      f = feature.getWorkspace().getFeature( (String) objCsData );

    return f;
  }
}
