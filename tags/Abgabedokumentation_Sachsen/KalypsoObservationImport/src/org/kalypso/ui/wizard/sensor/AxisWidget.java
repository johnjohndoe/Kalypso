package org.kalypso.ui.wizard.sensor;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SimpleAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

public class AxisWidget extends Composite
{
  private SimpleAxis m_axis = null;

  private Text m_textName;

  private Combo m_comboTypes;

  private boolean m_changeName = false;

  private boolean m_changeType = false;

  private Group m_group;

  /*
   * 
   * @author doemming
   */
  public AxisWidget( Composite parent, int style )
  {
    super( parent, style );
    setControl();
    updateGUIFromMember();
    validate();
  }

  private void setControl()
  {
    GridLayout gridLayout = new GridLayout();
    setLayout( gridLayout );
    GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.FILL;
    data2.verticalAlignment = GridData.FILL;
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    setLayoutData( data2 );

    m_group = new Group( this, SWT.NONE );
    gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    m_group.setLayout( gridLayout );
    m_group.setText( "Axe" );

    GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.verticalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    data1.grabExcessVerticalSpace = true;
    m_group.setLayoutData( data1 );

    Label labelName = new Label( m_group, SWT.NONE );
    labelName.setText( "Name" );
    GridData data = new GridData();
    data.horizontalAlignment = GridData.END;
    labelName.setLayoutData( data );

    m_textName = new Text( m_group, SWT.BORDER );
    m_textName.setText( "text2" );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    m_textName.setLayoutData( data );

    Label labelType = new Label( m_group, SWT.NONE );
    labelType.setText( "Art" );
    data = new GridData();
    data.horizontalAlignment = GridData.END;
    labelType.setLayoutData( data );

    m_comboTypes = new Combo( m_group, SWT.NONE );
    m_comboTypes.setItems( TimeserieConstants.TYPES_ALL );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    m_comboTypes.setLayoutData( data );
    pack();
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    super.dispose();
  }

  private void validate()
  {
    if( m_changeName )
      m_textName.setEditable( true );
    else
      m_textName.setEditable( false );
    if( m_changeType )
      m_comboTypes.setEnabled( true );
    else
      m_comboTypes.setEnabled( false );
    if( m_axis == null )
      setVisible( false );
    else
      setVisible( true );

  }

  public IAxis getAxis()
  {
    return m_axis;
  }

  public void setAxis( IAxis axis )
  {
    if( axis != null )
      m_axis = new SimpleAxis( axis );
    else
      m_axis = null;
    updateGUIFromMember();
    validate();
  }

  private void updateGUIFromMember()
  {
    if( m_axis != null )
    {
      m_textName.setText( m_axis.getName() );
      m_comboTypes.select( Arrays.binarySearch( TimeserieConstants.TYPES_ALL, m_axis.getType() ) );
    }
  }

  public void setMode( boolean changeName, boolean changeType )
  {
    m_changeName = changeName;
    m_changeType = changeType;
    validate();
  }
}