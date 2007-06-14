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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Default implementation of {@link IJunctionContext1DTo2D}
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings({"unchecked","hiding"})
public class JunctionContext1DTo2D extends JunctionContext1DToCLine implements IJunctionContext1DTo2D
{

  public JunctionContext1DTo2D( Feature featureToBind )
  {
    this( 
        featureToBind,
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_2D);    
  }

  public JunctionContext1DTo2D( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.JunctionContext1DToCLine#addElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  @Override
  public boolean addElementAsRef( IFE1D2DElement element )
  {
    Feature wrappedFeature = element.getWrappedFeature();
    if(TypeInfo.isPolyElementFeature( wrappedFeature ))
    {
      getFeature().setProperty( 
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D, 
          wrappedFeature.getId() );
      element.getContainers().addRef( this );
      return true;
    }
    else
    {
      return super.addElementAsRef(element);
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.JunctionContext1DToCLine#removeElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  @Override
  public boolean removeElementAsRef( IFE1D2DElement element )
  {
    Feature featureToDel = element.getWrappedFeature();
    if(TypeInfo.isPolyElementFeature( featureToDel ))
    {
      if(featureToDel.equals( getElement2D().getWrappedFeature() ))
      {
        element.getContainers().removeAllRefs( this );
        getFeature().setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D, 
            null );
        return true;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return super.removeElementAsRef(element);
    }
  }
  
  
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DTo2D#getElement2D()
   */
  public IPolyElement getElement2D( )
  {    
    final IPolyElement resolvedLink = 
      FeatureHelper.resolveLink( 
        this, 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D, 
        IPolyElement.class );
    return resolvedLink;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.JunctionContext1DToCLine#collectJuntionElements()
   */
  @Override
  protected List<IFE1D2DElement> collectJunctionElements( )
  {
    List<IFE1D2DElement> list = super.collectJunctionElements();
    IPolyElement element2D = getElement2D();
    if( element2D != null )
    {
      list.add( element2D );
    }
    return list;
  }
}
