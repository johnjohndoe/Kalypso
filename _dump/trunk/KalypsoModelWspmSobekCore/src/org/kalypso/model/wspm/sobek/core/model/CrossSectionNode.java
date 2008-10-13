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

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzone;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzonenDistances;
import org.kalypso.model.wspm.sobek.core.sperrzone.Sperrzone;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dirk Kuch
 */
public class CrossSectionNode extends AbstractNode implements ICrossSectionNode
{
  private Sperrzone m_sperrzone = null;

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
    final Feature feature = FeatureUtils.resolveFeature( getModel().getWorkspace(), objBranch );

    if( feature == null )
      return null;

    return new Branch( getModel(), feature );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getCrossSectionData()
   */
  public IProfil getProfile( )
  {
    IProfileFeature wspmProfile = getLinkedProfile();
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
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    final Feature linkedProfile = getLinkedProfile();
    final IBranch linkToBranch = getLinkToBranch();

    if( linkToBranch == null || linkedProfile == null )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode#getLinkedProfile()
   */
  public IProfileFeature getLinkedProfile( )
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

    return (IProfileFeature) f;
  }

  public enum DEFAULT_ROUGHNESS
  {
    eChezy,
    eManning,
    eStricklerKn,
    eStricklerKs,
    eWhiteColebrook,
    eNikuradse, // can't be set in Sobek GUI
    eEngelund, // can't be set in Sobek GUI
    eInherited, // can't be set in Sobek GUI, TODO roughness type = inherited?!?
    eDeBosAndBijkerk;

    public double getDefaultRoughness( )
    {
      final DEFAULT_ROUGHNESS defRoughness = DEFAULT_ROUGHNESS.valueOf( name() );
      switch( defRoughness )
      {
        case eChezy:
          return 45; // [m^(1/2)*s^(-1)]

        case eManning:
          return 0.03; // [s*m^(-1/3)]

        case eStricklerKn:
          return 0.2; // [m]

        case eStricklerKs:
          return 33; // [m^(1/3)*s^(-1)]

        case eWhiteColebrook:
          return 0.2; // [m]

        case eDeBosAndBijkerk:
          return 33.8; // [-]

        default:
          return Double.NaN;
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getSperrzone()
   */
  public ISperrzone getSperrzone( )
  {
    if( m_sperrzone == null )
    {
      m_sperrzone = new Sperrzone( getFeature() );
      try
      {
        final IBranch branch = getLinkToBranch();

        final GM_Point location = getLocation();
        final Geometry jtsLocation = JTSAdapter.export( location );
        final Geometry buffer = jtsLocation.buffer( ISperrzonenDistances.CROSS_SECTION_NODE );

        m_sperrzone.addSperrzone( branch, buffer );
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }

    return m_sperrzone;
  }

}
