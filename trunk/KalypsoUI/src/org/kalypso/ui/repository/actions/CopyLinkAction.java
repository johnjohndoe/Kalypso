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
package org.kalypso.ui.repository.actions;

import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.ObservationChooser;

/**
 * @author schlienger
 */
public class CopyLinkAction extends AbstractRepositoryExplorerAction
{
  public CopyLinkAction( final ObservationChooser explorer )
  {
    super( explorer, "Link kopieren", ImageProvider.IMAGE_OBSERVATION_LINK,
        "Kopiert den Link in der Zwischenablage für die ausgewählte Zeitreihe" );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IObservation obs = getExplorer().isObservationSelected( getExplorer().getSelection() );
    if( obs == null )
      return;

    final Clipboard clipboard = new Clipboard( getShell().getDisplay() );
    clipboard.setContents( new Object[]
    { obs.getIdentifier() }, new Transfer[]
    { TextTransfer.getInstance() } );
    clipboard.dispose();
  }
}
