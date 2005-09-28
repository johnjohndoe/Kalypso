/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.view.wq;

import org.eclipse.compare.internal.ResizableDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.ogc.sensor.view.wq.diagram.WQRelationDiagramViewer;
import org.kalypso.ogc.sensor.view.wq.table.WQRelationTableViewer;

/**
 * @author schlienger
 */
public class WQRelationDialog extends ResizableDialog
{
  private final WQTableSet m_wqs;
  private final String m_title;

  public WQRelationDialog( final Shell parentShell, final String title, final WQTableSet wqs )
  {
    super( parentShell, null );
    
    m_title = title;
    m_wqs = wqs;
  }

  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite)super.createDialogArea( parent );

    final SashForm form = new SashForm( composite, SWT.HORIZONTAL );
    form.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final WQRelationDiagramViewer diagViewer = new WQRelationDiagramViewer( form );
    final WQRelationTableViewer tableViewer = new WQRelationTableViewer( form );

    try
    {
      diagViewer.setInput( m_wqs );
      tableViewer.setInput( m_wqs );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      MessageDialog.openError( getShell(), "WQ-Beziehung, Dialog öffnen", e.getLocalizedMessage() );
    }
    
    getShell().setText( m_title );

    return composite;
  }
}
