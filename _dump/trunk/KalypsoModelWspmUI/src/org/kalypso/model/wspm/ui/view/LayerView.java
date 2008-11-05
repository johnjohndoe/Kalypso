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
package org.kalypso.model.wspm.ui.view;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;

/**
 * @author kimwerner
 */

public class LayerView extends ViewPart

{

  private ScrolledForm m_form = null;

  private FormToolkit m_toolkit;

  private IChartLayer m_activeLayer;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_form != null && m_form.getBody() != null )
      m_form.getBody().setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = new FormToolkit( parent.getDisplay() );
    m_form = m_toolkit.createScrolledForm( parent );
    m_toolkit.decorateFormHeading( m_form.getForm() );
    m_form.getForm().setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
    final GridLayout bodyLayout = new GridLayout();
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    m_form.getForm().getBody().setLayout( bodyLayout );

  }

  public final void updatePanel( final IChartLayer activeLayer )
  {
    if( m_activeLayer == activeLayer )
      return;

    if( m_form == null || m_form.isDisposed() || m_form.getBody() == null )
      return;
    for( final Control ctrl : m_form.getBody().getChildren() )
    {
      if( !ctrl.isDisposed() )
        ctrl.dispose();
    }
    if( activeLayer != null )
    {
      m_form.getForm().setMessage( "", 0 );
      m_form.getForm().setText( activeLayer.getTitle() );
      final IProfilView panel = activeLayer instanceof IProfilChartLayer ? ((IProfilChartLayer) activeLayer).createLayerPanel() : null;

      if( panel != null )
      {
        final Control control = panel.createControl( m_form.getForm().getBody(), m_toolkit );

        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }

      m_activeLayer = activeLayer;
    }
    else
    {
      m_form.getForm().setText( "" );
      m_form.getForm().setMessage( Messages.TableView_9, IMessageProvider.INFORMATION );
    }

    m_form.getForm().layout();

  }
}
