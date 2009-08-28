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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.model;

import java.math.BigDecimal;
import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;

/**
 * @author Gernot Belger
 */
public class SurfacePolygonWizardPage extends WizardPage
{
  private final SurfacePolygonSymbolizer m_symbolizer;

  private final CreateModelTinWizardPage m_exportPage;

  private BigDecimal m_minValue = null;

  private BigDecimal m_maxValue = null;

  
  public SurfacePolygonWizardPage( final String pageName, final SurfacePolygonSymbolizer symbolizer, final CreateModelTinWizardPage exportPage )
  {
    super( pageName, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.SurfacePolygonWizardPage.0"), null ); //$NON-NLS-1$
    
    m_symbolizer = symbolizer;
    m_exportPage = exportPage;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    m_minValue = m_exportPage.getMin();
    m_maxValue = m_exportPage.getMax();
    
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new FillLayout() );

    final PolygonColorMap colorMap = m_symbolizer.getColorMap();
    final PolygonColorMapEntry[] colorMapEntries = colorMap.getColorMap();
    final PolygonColorMapEntry fromEntry = colorMapEntries[0];
    final PolygonColorMapEntry toEntry = colorMapEntries[colorMapEntries.length - 1];

    new PolygonColorMapEditorComposite( panel, SWT.NONE, fromEntry, toEntry, m_minValue, m_maxValue )
    {
      @Override
      protected void colorMapChanged( )
      {
        final List<PolygonColorMapEntry> colorMapList = getColorMap();
        if( colorMapList.size() > 0 )
          colorMap.replaceColorMap( colorMapList );
      }
    };

    setControl( panel );
  }
}
