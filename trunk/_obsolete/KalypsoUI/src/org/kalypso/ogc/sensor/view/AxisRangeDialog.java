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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;

public class AxisRangeDialog extends ResizableDialog
{

  private final String m_axisType;

  private Text m_minText;

  private Text m_intText;

  private Text m_countText;

  private Text m_defaultText;

  private Text m_nameText;

  private final FocusListener m_focusListener = new FocusListener()
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

  private Object m_int = null;

  private Object m_min = null;

  private boolean m_valid = false;

  private Object m_default = null;

  private Object m_name = null;

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
  @Override
  protected Control createDialogArea( Composite parent )
  {
    getShell().setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.0") ); //$NON-NLS-1$
    Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 2, true ) );
    // Name
    Label label6 = new Label( group, SWT.NONE );
    label6.setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.1") ); //$NON-NLS-1$
    m_nameText = new Text( group, 2050 );
    m_nameText.setText( "" ); //$NON-NLS-1$
    GridData m_nameData = new GridData( GridData.END );
    m_nameData.widthHint = 150;
    m_nameText.setLayoutData( m_nameData );
    // MIN
    Label label = new Label( group, SWT.NONE );
    label.setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.3") ); //$NON-NLS-1$
    m_minText = new Text( group, 2050 );
    m_minText.setText( "0" ); //$NON-NLS-1$
    GridData m_minData = new GridData( GridData.END );
    m_minData.widthHint = 150;
    m_minText.setLayoutData( m_minData );
    // Intervall
    Label label2 = new Label( group, SWT.NONE );
    label2.setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.5") ); //$NON-NLS-1$
    m_intText = new Text( group, 2050 );
    m_intText.setText( "1" ); //$NON-NLS-1$
    GridData m_intData = new GridData( GridData.END );
    m_intData.widthHint = 150;
    m_intText.setLayoutData( m_intData );
    // ANZAHL
    Label label3 = new Label( group, SWT.NONE );
    label3.setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.7") ); //$NON-NLS-1$
    m_countText = new Text( group, 2050 );
    m_countText.setText( "10" ); //$NON-NLS-1$
    GridData m_countData = new GridData( GridData.END );
    m_countData.widthHint = 150;
    m_countText.setLayoutData( m_countData );
    //Default Wert
    Label label5 = new Label( group, SWT.NONE );
    label5.setText( Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.9") ); //$NON-NLS-1$
    m_defaultText = new Text( group, 2050 );
    m_defaultText.setText( "10" ); //$NON-NLS-1$
    GridData m_defaultData = new GridData( GridData.END );
    m_defaultData.widthHint = 150;
    m_defaultText.setLayoutData( m_defaultData );

    m_minText.addFocusListener( m_focusListener );
    m_intText.addFocusListener( m_focusListener );
    m_countText.addFocusListener( m_focusListener );
    m_defaultText.addFocusListener( m_focusListener );
    m_nameText.addFocusListener( m_focusListener );

    Label label4 = new Label( group, SWT.NONE );
    label4.setText( "" ); //$NON-NLS-1$
    m_validText = new Text( group, SWT.READ_ONLY );
    GridData m_validData = new GridData( GridData.END );
    m_validData.widthHint = 150;
    m_validText.setLayoutData( m_validData );
    m_minText.addFocusListener( m_focusListener );
    validate();
    return group;
  }

  public Object getMin()
  {
    return m_min;
  }

  public Object getInt()
  {
    return m_int;

  }

  public Object getDefault()
  {
    return m_default;

  }

  public Object getName()
  {
    return m_name;

  }

  public int getCount()
  {
    return m_count;
  }

  public void validate()
  {
    String message = ""; //$NON-NLS-1$
    try
    {
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.13"); //$NON-NLS-1$
      m_name = m_nameText.getText();
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.14"); //$NON-NLS-1$
      m_min = SpecialPropertyMapper.cast( m_minText.getText(), TimeserieUtils.getDataClass( m_axisType ), false, true );
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.15"); //$NON-NLS-1$
      m_int = SpecialPropertyMapper.cast( m_intText.getText(), TimeserieUtils.getDataClass( m_axisType ), false, true );
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.16"); //$NON-NLS-1$
      m_count = ( (Integer)SpecialPropertyMapper.cast( m_countText.getText(), Integer.class, false, true ) ).intValue();
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.17"); //$NON-NLS-1$
      m_default = SpecialPropertyMapper.cast( m_defaultText.getText(), TimeserieUtils.getDataClass( m_axisType ),
          false, true );
      message = Messages.getString("org.kalypso.ogc.sensor.view.AxisRangeDialog.18"); //$NON-NLS-1$
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
