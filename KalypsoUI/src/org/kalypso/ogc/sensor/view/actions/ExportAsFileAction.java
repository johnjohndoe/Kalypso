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
package org.kalypso.ogc.sensor.view.actions;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.ogc.sensor.view.wizard.ExportAsFileWizard;
import org.kalypso.ui.ImageProvider;

/**
 * @author schlienger
 */
public class ExportAsFileAction extends AbstractObservationChooserAction implements ISelectionChangedListener
{
  public ExportAsFileAction( final ObservationChooser explorer )
  {
    super( explorer, "Datei herunterladen", ImageProvider.IMAGE_ZML_DOWNLOAD,
        "Lädt die selektierte Zeitreihe als lokale Datei herunter" );

    explorer.addSelectionChangedListener( this );
    setEnabled( explorer.isObservationSelected( explorer.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IObservation obs = getExplorer().isObservationSelected( getExplorer().getSelection() );
    if( obs == null )
      return;

    final WizardDialog dialog = new WizardDialog( getShell(), new ExportAsFileWizard( obs ) );
    dialog.open();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isObservationSelected( event.getSelection() ) != null );
  }
}
