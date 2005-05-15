package org.kalypso.ogc.gml.map.widgets.editrelation;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FindExistingHeavyRelationsFeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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

public class HeavyRelationType implements IRelationType
{

  private RelationType m_relationType1;

  private RelationType m_relationType2;

  /**
   * @author doemming
   */
  public HeavyRelationType( FeatureType ft1, FeatureAssociationTypeProperty linkFTP1,
      FeatureType ft2, FeatureAssociationTypeProperty linkFTP2, FeatureType ft3 )
  {
    m_relationType1 = new RelationType( ft1, linkFTP1, ft2 );
    m_relationType2 = new RelationType( ft2, linkFTP2, ft3 );
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#fitsTypes(org.kalypsodeegree.model.feature.FeatureType,
   *      org.kalypsodeegree.model.feature.FeatureType)
   */
  public boolean fitsTypes( FeatureType f1, FeatureType f2 )
  {    
    if( f1 == null || f2 == null )
      return false;
    return m_relationType1.getSrcFT() == f1 && m_relationType2.getDestFT() == f2;
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getFitProblems(org.kalypsodeegree.model.feature.GMLWorkspace, org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.model.feature.Feature, boolean)
   */
  public String getFitProblems( GMLWorkspace workspace, Feature f1, Feature f2, boolean isAddMode )
  {
    FindExistingHeavyRelationsFeatureVisitor visitor = new FindExistingHeavyRelationsFeatureVisitor( workspace,this );
    visitor.visit( f1 );
    boolean exists = visitor.relationExistsTo( f2 );
    if( !isAddMode )
      return exists ? null : "Relation existiert nicht";
    // is addmode:
    if( exists )
      return "Relation existiert bereits";
    return m_relationType1.getFitProblemsfromOccurency( f1, isAddMode );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getDestFT()
   */
  public FeatureType getDestFT()
  {
    return m_relationType2.getDestFT();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getSrcFT()
   */
  public FeatureType getSrcFT()
  {
    return m_relationType1.getSrcFT();
  }

  public FeatureAssociationTypeProperty getLink1()
  {
    return m_relationType1.getLink();
  }

  public FeatureAssociationTypeProperty getLink2()
  {
    return m_relationType2.getLink();
  }

  public FeatureType getBodyFT()
  {
    return m_relationType1.getDestFT();
  }

  //  private final FeatureType m_bodyFT;
  //
  //  private final FeatureAssociationTypeProperty m_destLinkFTP;
  //
  //  /*
  //
  //  public FeatureType getBodyFT()
  //  {
  //    return m_bodyFT;
  //  }
  //
  //  public FeatureType getDestFT()
  //  {
  //    return m_destFT;
  //  }
  //
  //  public FeatureAssociationTypeProperty getDestLinkFTP()
  //  {
  //    return m_destLinkFTP;
  //  }
  //
  //  public FeatureType getSrcFT()
  //  {
  //    return m_srcFT;
  //  }
  //
  //  public boolean equals( Object obj )
  //  {
  //    if( obj == null || !( obj instanceof HeavyRelationType ) )
  //      return false;
  //    final HeavyRelationType other = (HeavyRelationType)obj;
  //    return super.equals( obj ) && other.getBodyFT().equals( m_bodyFT )
  //        && other.getDestLinkFTP().equals( m_destLinkFTP );
  //  }
  //
  //  public int hashCode()
  //  {
  //    return ( getSrcFT().getName() + getLink().getName() + m_bodyFT.getName()
  //        + m_destLinkFTP.getName() + getDestFT().getName() ).hashCode();
  //  }
}