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
package org.kalypso.project.database.client.ui.project.wizard.imp;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;

/**
 * Composite for calling the new project wizard
 * 
 * @author Dirk Kuch
 */
public class SpecialImportProjectComposite extends Composite
{
  public static Image IMG_IMPORT = new Image( null, SpecialImportProjectComposite.class.getResourceAsStream( "icons/project_import.gif" ) );

  private final FormToolkit m_toolkit;

  protected final IKalypsoModuleEnteringPageHandler m_enteringPage;

  public SpecialImportProjectComposite( final Composite parent, final FormToolkit toolkit, final IKalypsoModuleEnteringPageHandler enteringPage )
  {
    super( parent, SWT.NULL );

    m_toolkit = toolkit;
    m_enteringPage = enteringPage;

    final GridLayout layout = new GridLayout();
    layout.verticalSpacing = layout.marginWidth = 0;
    this.setLayout( layout );

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public void update( )
  {
    if( this.isDisposed() )
      return;

    final ImageHyperlink lnkImport = m_toolkit.createImageHyperlink( this, SWT.NULL );
    lnkImport.setImage( IMG_IMPORT );
    lnkImport.setText( m_enteringPage.getImportWizardLabel() );

    lnkImport.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final WizardDialog2 dialog = new WizardDialog2( PlatformUI.getWorkbench().getDisplay().getActiveShell(), m_enteringPage.getImportWizard() );
        dialog.setRememberSize( true );

        dialog.open();
      }
    } );

    m_toolkit.adapt( this );
    this.layout();
  }
}
