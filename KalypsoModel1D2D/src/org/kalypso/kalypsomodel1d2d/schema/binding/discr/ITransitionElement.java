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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;

public interface ITransitionElement extends IAbstractJunction
{
  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "TransitionElement" ); //$NON-NLS-1$

  public static final QName PROP_CONTI_LINES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "continuityLine" ); //$NON-NLS-1$

  public static final QName PROP_TRANSITION_TYPE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "transitionType" ); //$NON-NLS-1$

  public static enum TRANSITION_TYPE
  {
    TYPE1D2D
    {
      @Override
      final String getValue( )
      {
        return "1D2D"; //$NON-NLS-1$
      }
    },
    TYPE2D1D
    {
      @Override
      final String getValue( )
      {
        return "2D1D"; //$NON-NLS-1$
      }
    };

    abstract String getValue( );
  }

  public void setTransitionType( final TRANSITION_TYPE transitionType );

  public TRANSITION_TYPE getTransitionType( );
}
