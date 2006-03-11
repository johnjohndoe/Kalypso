/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.wizard.ocs;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.dialogs.WizardExportResourcesPage;

/**
 * OcsExportWizardResourcesPage
 * 
 * @author schlienger
 */
public class OcsExportWizardResourcesPage extends WizardExportResourcesPage
{
  protected Button m_guessIdentifiersCheckBox;

  //  private final static String STORE_GUESS_IDS = "OcsExportWizardResourcesPage.STORE_GUESS_IDS"; //$NON-NLS-1$

  public OcsExportWizardResourcesPage( final IStructuredSelection selection )
  {
    super( "Zeitreihen Selektion", selection );

    setTitle( "Zeitreihen Selektion" );
    setDescription( "Wählen Sie die Dateien die auf den Server zurückgespeichert werden sollen." );
  }

  /**
   * @see org.eclipse.ui.dialogs.WizardExportResourcesPage#createDestinationGroup(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createDestinationGroup( Composite parent )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.dialogs.WizardDataTransferPage#createOptionsGroupButtons(org.eclipse.swt.widgets.Group)
   */
  @Override
  protected void createOptionsGroupButtons( Group optionsGroup )
  {
    m_guessIdentifiersCheckBox = new Button( optionsGroup, SWT.CHECK | SWT.LEFT );
    m_guessIdentifiersCheckBox.setText( "Kennzeichen aus Metadaten benutzen" );

    m_guessIdentifiersCheckBox.setSelection( true );
  }

  /**
   * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
   */
  public void handleEvent( Event event )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.dialogs.WizardExportResourcesPage#getSelectedResources()
   */
  @Override
  public List getSelectedResources( )
  {
    return super.getSelectedResources();
  }
}