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

package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This feature control is a combo box, which just sets the feature-value to the given value when selected.
 * <p>
 * Today only properties with String type are supported.
 * </p>
 * 
 * @author belger
 */
public class ComboFeatureControl extends AbstractFeatureControl
{
  private final SelectionListener m_listener = new SelectionListener()
  {
    public void widgetSelected( final SelectionEvent e )
    {
      comboSelected();
    }

    public void widgetDefaultSelected( final SelectionEvent e )
    {
      comboSelected();
    }
  };

  private final List<ModifyListener> m_listeners = new ArrayList<ModifyListener>( 5 );

  private Combo m_combo = null;

  private final String[] m_labels;
  private final Object[] m_values;

  public ComboFeatureControl( final IPropertyType ftp, final String[] labels, final Object[] values )
  {
    this( null, ftp, labels, values );
  }

  public ComboFeatureControl( final Feature feature, final IPropertyType ftp, final String[] labels, final Object[] values )
  {
    super( feature, ftp );

    m_labels = labels;
    m_values = values;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_combo != null && !m_combo.isDisposed() )
      m_combo.removeSelectionListener( m_listener );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_combo = new Combo( parent, style );

    m_combo.setItems( m_labels );

    m_combo.addSelectionListener( m_listener );

    updateControl();

    return m_combo;
  }

  protected void comboSelected( )
  {
    final Feature feature = getFeature();
    final IPropertyType pt = getFeatureTypeProperty();

    final Object oldValue = getCurrentFeatureValue();
    final Object newValue = m_values[m_combo.getSelectionIndex()];

    if( !newValue.equals( oldValue ) )
      fireFeatureChange( new FeatureChange( feature, pt, newValue ) );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    final Object currentFeatureValue = getCurrentFeatureValue();

    for( int i = 0; i < m_values.length; i++ )
    {
      if( m_values[i].equals( currentFeatureValue ) )
      {
        if( i != m_combo.getSelectionIndex() )
          m_combo.select( i );
        break;
      }
    }
  }

  /** Returns the current value of the feature as string. */
  private Object getCurrentFeatureValue( )
  {
    return getFeature().getProperty( getFeatureTypeProperty() );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    // a radio button is always valid
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( ModifyListener l )
  {
    m_listeners.remove( l );
  }
}
