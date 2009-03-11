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
package org.kalypso.ogc.gml.featureview.dialog;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Text;

/**
 * This is a listener for the EnvelopeDialog text fields. It parses the text to doubles and sets it to the corresponding
 * double field.
 * 
 * @author albert
 */
final class PointDialogListener implements ModifyListener
{
  private PointDialog m_ptDialog;

  private int m_whichText;

  public PointDialogListener( PointDialog ptDialog, int whichText )
  {
    m_ptDialog = ptDialog;
    m_whichText = whichText;
  }

  public void modifyText( ModifyEvent e )
  {
    final Text text = (Text) e.getSource();
    final String content = text.getText();

    double[] values = m_ptDialog.getValues();

    try
    {
      double dbl = Double.parseDouble( content );

      values[m_whichText] = dbl;

      m_ptDialog.setValues( values );
    }
    catch( Exception ex )
    {
      values[m_whichText] = new Double( "0.0" ); //$NON-NLS-1$

      m_ptDialog.setValues( values );
    }
    finally
    {
      /* Setze den Status des OK-Buttons. */
      m_ptDialog.checkModified();
    }
  }
}