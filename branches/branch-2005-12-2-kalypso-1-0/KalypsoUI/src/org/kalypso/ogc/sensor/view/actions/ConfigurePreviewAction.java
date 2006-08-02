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

import java.text.DateFormat;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.window.Window;
import org.kalypso.contribs.eclipse.jface.dialogs.DateRangeInputDialog;
import org.kalypso.contribs.eclipse.swt.widgets.DateRangeInputControlStuct;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Configure preview daterange for current <code>IRepository</code>.
 * 
 * @author schlienger
 */
public class ConfigurePreviewAction extends AbstractObservationChooserAction implements ISelectionChangedListener
{
  public ConfigurePreviewAction( final ObservationChooser explorer )
  {
    super( explorer, "Einstellungen", ImageProvider.IMAGE_ZML_REPOSITORY_CONF,
        "Einstellungen der Zeitreihen-Vorschau setzen" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  public void run()
  {
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    final DateFormat dateFormat = TimeserieUtils.getDateFormat();
    final DateRangeInputDialog dlg = new DateRangeInputDialog( getShell(), "Zeitraum-Eingabe",
        "Bitte geben Sie einen Zeitraum ein.", DateRangeInputControlStuct.create( rep.getProperties(), dateFormat ) );

    if( dlg.open() == Window.OK )
    {
      // save dialog settings for next dialog call
      final DateRangeInputControlStuct struct = dlg.getStruct();
      struct.save( KalypsoGisPlugin.getDefault().getDialogSettings() );

      // save properties of the repository
      struct.save( rep.getProperties() );

      // clear cache in order to refetch values from server in case date range
      // changed
      ObservationCache.clearCache();
    }
  }

  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }
}