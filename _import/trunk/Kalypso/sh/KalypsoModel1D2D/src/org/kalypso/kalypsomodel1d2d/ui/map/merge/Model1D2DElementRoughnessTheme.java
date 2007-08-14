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
package org.kalypso.kalypsomodel1d2d.ui.map.merge;

import java.awt.Graphics;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Theme that shows elements with rougness
 * 
 * @author Patrice Congo
 * 
 */
public class Model1D2DElementRoughnessTheme extends AbstractKalypsoTheme
{

  private FERoughnessDisplayElement feRoughnessDisplayElement;

  private IStaticModel1D2D staticModel;

  public Model1D2DElementRoughnessTheme( final String name, final IMapModell mapModel )
  {
    super( name, "Elemente+Rauhheiten", mapModel );
  }

  public void setStaticModel( final IStaticModel1D2D staticModel )
  {
    this.staticModel = staticModel;
    if( staticModel != null )
    {
      feRoughnessDisplayElement = new FERoughnessDisplayElement( staticModel );
    }
    else
    {
      feRoughnessDisplayElement = null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    if( staticModel == null )
    {
      return null;
    }
    final IFEDiscretisationModel1d2d discrModel = staticModel.getDiscretisationModel();
    if( discrModel == null )
    {
      return null;
    }
    final IFeatureWrapperCollection<IFE1D2DElement> elements = discrModel.getElements();
    final GM_Envelope bbox = elements.getWrappedList().getBoundingBox();

    return bbox;

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor )
  {
    if( selected )
    {
      return;
    }
    if( feRoughnessDisplayElement != null )
    {
      feRoughnessDisplayElement.paint( g, p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return super.isLoaded();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getType()
   */
  @Override
  public String getType( )
  {
    return "GML_MERGE";
  }

}
