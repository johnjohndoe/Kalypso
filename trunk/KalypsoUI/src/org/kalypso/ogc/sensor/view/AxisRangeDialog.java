/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.view;

import java.util.ResourceBundle;

import org.eclipse.compare.internal.ResizableDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;

public class AxisRangeDialog extends ResizableDialog
{

  private final String m_axisType;

  private Text m_minText;

  private Text m_maxText;

  private Text m_countText;

  private final FocusListener m_focusListsner = new FocusListener()
  {
    public void focusGained( FocusEvent e )
    {
    // nothing
    }

    public void focusLost( FocusEvent e )
    {
      validate();
    }
  };

  private Text m_validText;

  private int m_count = 1;

  private Object m_max = null;

  private Object m_min = null;

  private boolean m_valid = false;

  /**
   *  
   */
  public AxisRangeDialog( Shell parent, ResourceBundle bundle, String axisType )
  {
    super( parent, bundle );
    m_axisType = axisType;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( Composite parent )
  {
    getShell().setText( "Axis" );

    Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );
    // MIN
    Label label = new Label( group, SWT.NONE );
    label.setText( "min" );
    m_minText = new Text( group, SWT.NONE );
    m_minText.setText( "0" );
    // MAX
    Label label2 = new Label( group, SWT.NONE );
    label2.setText( "max" );
    m_maxText = new Text( group, SWT.NONE );
    m_maxText.setText( "10" );
    // ANZAHL
    Label label3 = new Label( group, SWT.NONE );
    label3.setText( "Anzahl" );
    m_countText = new Text( group, SWT.NONE );
    m_countText.setText( "10" );
    m_minText.addFocusListener( m_focusListsner );
    m_maxText.addFocusListener( m_focusListsner );
    m_countText.addFocusListener( m_focusListsner );

    Label label4 = new Label( group, SWT.NONE );
    label4.setText( "" );
    m_validText = new Text( group, SWT.READ_ONLY );
    m_minText.addFocusListener( m_focusListsner );
    validate();
    return group;
  }

  public Object getMin()
  {
    return m_min;
  }

  public Object getMax()
  {
    return m_max;
  }

  public int getCount()
  {
    return m_count;
  }

  public void validate()
  {
    String message = "";
    try
    {
      message = "Min ist nicht korrekt";
      m_min = SpecialPropertyMapper.cast( m_minText.getText(), TimeserieUtils.getDataClass( m_axisType ), false, true );
      message = "Max ist nicht korrekt";
      m_max = SpecialPropertyMapper.cast( m_maxText.getText(), TimeserieUtils.getDataClass( m_axisType ), false, true );
      message = "Anzahl ist nicht korrekt";
      m_count = ( (Integer)SpecialPropertyMapper.cast( m_countText.getText(), Integer.class, false, true ) ).intValue();
      message = "Eingabe ist OK";
      m_valid = true;
    }
    catch( Exception e )
    {
      m_valid = false;
    }
    finally
    {
      m_validText.setText( message );
    }
  }

  public boolean isValid()
  {
    return m_valid;
  }
}
