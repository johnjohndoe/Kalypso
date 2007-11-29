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

import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkStructure extends AbstractNode implements ISbkStructure
{

  public SbkStructure( final IModelMember model, final Feature node )
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
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return TYPE.eSbkStructure;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructure#getLinkToBranch()
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

}
