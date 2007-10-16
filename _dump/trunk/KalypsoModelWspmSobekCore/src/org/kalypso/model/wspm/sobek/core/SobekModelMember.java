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
package org.kalypso.model.wspm.sobek.core;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker;
import org.kalypso.model.wspm.sobek.core.interfaces.ICalculationLink;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfallMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.model.wspm.sobek.core.model.BranchMaker;
import org.kalypso.model.wspm.sobek.core.pub.FNNodeUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public final class SobekModelMember implements ISobekModelMember
{
  private Feature m_modelMember;

  private static ISobekModelMember m_model = null;

  private SobekModelMember( Feature modelMember )
  {
    if( modelMember == null )
      throw new IllegalStateException( "modelMember is null" );

    if( !ISobekConstants. QN_SOBEK_MODEL_MEMBER.equals( modelMember.getFeatureType().getQName() ) )
      throw new IllegalStateException( "modelMember is not of type: " + ISobekConstants.QN_SOBEK_MODEL_MEMBER);

    if( m_model == null )
    {
    m_modelMember = modelMember;
      m_model = this;
    }

  }

  public static ISobekModelMember getModel( Feature feature )
  {
    if( m_model == null && feature != null )
    {
      m_model = new SobekModelMember( feature );
      return m_model;
    }

    return m_model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMembers()
   */
  public IBranch[] getBranchMembers( )
  {
    List<IBranch> myBranches = new ArrayList<IBranch>();

    List< ? > branches = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
    for( Object object : branches )
    {
      if( !(object instanceof Feature) )
        continue;

      Feature branch = (Feature) object;

      IBranch myBranch = new Branch( this, branch );
      myBranches.add( myBranch );
    }

    return myBranches.toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getCalculationLinkMembers()
   */
  public ICalculationLink[] getCalculationLinkMembers( )
  {
    throw (new NotImplementedException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getLastfallMembers()
   */
  public ILastfallMember[] getLastfallMembers( )
  {
    throw (new NotImplementedException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getNodeMembers()
   */
  public INode[] getNodeMembers( )
  {
    List<INode> myNodes = new ArrayList<INode>();

    List< ? > nodes = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    for( Object object : nodes )
    {
      if( !(object instanceof Feature) )
        continue;

      Feature node = (Feature) object;
      myNodes.add( FNNodeUtils.getNode( this, node ) );
    }

    return myNodes.toArray( new INode[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMaker()
   */
  public IBranchMaker getBranchMaker( )
  {
    return new BranchMaker( this );
  }

  public Feature getFeature( )
  {
    return m_modelMember;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#deleteFoo(org.kalypsodeegree.model.feature.Feature)
   */
  public void deleteFoo( Feature feature ) throws Exception
  {
    QName qn = feature.getFeatureType().getQName();

    if( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH.equals( qn ) )
      new Branch( this, feature ).delete();
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qn ) )
      FeatureUtils.deleteFeature( feature );
    else
      throw (new NotImplementedException());
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember#writePi(java.net.URL,
   *      org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember.TARGET)
   * @author thuel2
   */
  public void writePi( final URL targetDir, final TARGET target )
  {
    if( TARGET.eLocations.equals( target ) )
    {
      final INode[] nodes = getNodeMembers();
      // create target file (outputStream)
      // write
    }
    throw (new NotImplementedException());
  }

}
