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
package org.kalypso.model.wspm.ui.view.legend;

import org.kalypso.chart.ui.editor.ChartEditorTreeContentProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;

import de.openali.odysseus.chart.ext.base.layer.AbstractExpandableLayer;
import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;

/**
 * @author kimwerner
 */
public class ProfilChartEditorTreeContentProvider extends ChartEditorTreeContentProvider
{

  private final IExpandableChartLayer m_modelLayer;
  
  public ProfilChartEditorTreeContentProvider( final IChartModel model )
  {
    super( model );
     m_modelLayer = new AbstractExpandableLayer()
    {
  
      /**
       * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getTitle()
       */
      @Override
      public String getTitle( )
      {
        return getModel().getTitle();
      }

      /**
       * @see de.openali.odysseus.chart.ext.base.layer.AbstractExpandableLayer#getLayerManager()
       */
      @Override
      public ILayerManager getLayerManager( )
      {
        return getModel().getLayerManager();
      }
    };
    
    m_modelLayer.setVisible( true );

  }

  /**
   * @see org.kalypso.chart.ui.editor.ChartEditorTreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    final Object parent = super.getParent( element );
    if( parent == getModel() )
      return m_modelLayer;
    return parent;
  }

  /**
   * @see org.kalypso.chart.ui.editor.ChartEditorTreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object element )
  {
    if( element instanceof IChartModel )
    {
      return new IExpandableChartLayer[] { m_modelLayer };
    }
    if( element instanceof AbstractProfilTheme )
    {
      return ((AbstractProfilTheme)element).getLegendNodes();
    }
    return super.getChildren( element );
  }

  /**
   * @see org.kalypso.chart.ui.editor.ChartEditorTreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    if( element instanceof IChartModel )
    {
      return true;
    }
    if( element instanceof AbstractProfilTheme )
    {
      return ((AbstractProfilTheme)element).getLegendNodes().length > 0;
    }
    return super.hasChildren( element );
  }

}
