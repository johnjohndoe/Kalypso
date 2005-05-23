/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ui.repository.view;

import org.eclipse.compare.internal.ResizableDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author schlienger (23.05.2005)
 */
public class ObservationChooserDialog extends ResizableDialog
{
  private ObservationChooser m_chooser;

  public ObservationChooserDialog( final Shell parent )
  {
    super( parent, null );
  }

  protected Control createDialogArea( Composite parent )
  {
    final Composite composite = (Composite)super.createDialogArea( parent );

    composite.setLayout( new FillLayout() );
    m_chooser = new ObservationChooser( composite, getShell() );

    return composite;
  }
  
  public String getSelectedObservation()
  {
    if( m_chooser != null )
    {
      final Object sel = ( (StructuredSelection)m_chooser.getSelection() )
          .getFirstElement();

      if( sel instanceof IObservation )
      {
        final IObservation obs = (IObservation)sel;
        return obs.getIdentifier();
      }
    }
    
    return null;
  }

  public void setSelectedObservation( String id )
  {
    if( m_chooser != null )
    {
      // TODO select the observation in the tree
    }
  }
}
