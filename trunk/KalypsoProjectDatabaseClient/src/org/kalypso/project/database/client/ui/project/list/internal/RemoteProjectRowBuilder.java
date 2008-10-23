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
package org.kalypso.project.database.client.ui.project.list.internal;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.project.database.client.ui.project.wizard.info.WizardInfoRemoteProject;
import org.kalypso.project.database.common.interfaces.IKalypsoProject;

/**
 * @author Dirk Kuch
 */
public class RemoteProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{
  protected final IKalypsoProject m_bean;

  public RemoteProjectRowBuilder( final IKalypsoProject bean )
  {
    m_bean = bean;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {

    final Composite body = toolkit.createComposite( parent );
    body.setLayout( new GridLayout( 2, false ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
    lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    lnk.setImage( IMG_REMOTE_PROJECT );
    lnk.setText( m_bean.getName() );

    /* info */
    final ImageHyperlink lnkInfo = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkInfo.setImage( IMG_REMOTE_INFO );
    lnkInfo.setToolTipText( String.format( "Projektinformationen: %s", m_bean.getName() ) );

    lnkInfo.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final WizardInfoRemoteProject wizard = new WizardInfoRemoteProject( m_bean );

        final WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.open();
      }
    } );

  }

}
