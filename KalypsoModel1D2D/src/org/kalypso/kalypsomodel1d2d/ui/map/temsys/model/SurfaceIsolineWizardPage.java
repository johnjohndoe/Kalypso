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

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.ui.editor.sldEditor.LineColorMapEditorComposite;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;

/**
 * @author Gernot Belger
 */
public class SurfaceIsolineWizardPage extends WizardPage
{
  private final SurfaceLineSymbolizer m_symbolizer;

  private final CreateModelTinWizardPage m_exportPage;

  protected SurfaceIsolineWizardPage( final String pageName, final SurfaceLineSymbolizer symbolizer, final CreateModelTinWizardPage exportPage )
  {
    super( pageName, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.SurfaceIsolineWizardPage0" ), null ); //$NON-NLS-1$
    m_symbolizer = symbolizer;
    m_exportPage = exportPage;

    setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.SurfaceIsolineWizardPage1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final BigDecimal minValue = m_exportPage.getMin();
    final BigDecimal maxValue = m_exportPage.getMax();

    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new FillLayout() );

    final LineColorMap colorMap = m_symbolizer.getColorMap();
    new LineColorMapEditorComposite( panel, SWT.NONE, colorMap, minValue, maxValue );

    setControl( panel );
  }
}