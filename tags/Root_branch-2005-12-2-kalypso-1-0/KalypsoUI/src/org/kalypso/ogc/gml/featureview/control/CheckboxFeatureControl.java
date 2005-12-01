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
package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.modfier.BooleanModifier;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author belger
 */
public class CheckboxFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private Button m_checkbox = null;

  private final IFeatureModifier m_modifier;

  private Collection m_modlistener = new ArrayList();

  //  public CheckboxFeatureControl( final GMLWorkspace workspace, final FeatureTypeProperty ftp )
  //  {
  //    this( workspace, null, ftp );
  //  }

  public CheckboxFeatureControl( final GMLWorkspace workspace, final Feature feature, final FeatureTypeProperty ftp )
  {
    super( workspace, feature, ftp );

    m_modifier = new BooleanModifier( ftp );
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    super.dispose();

    if( m_checkbox != null )
      m_checkbox.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_checkbox = new Button( parent, style | SWT.CHECK );

    m_checkbox.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( SelectionEvent e )
      {
        fireFeatureChange( getChange() );
        fireModified();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        fireFeatureChange( getChange() );
        fireModified();
      }
    } );

    updateControl();

    return m_checkbox;
  }

  /**
   * Checkbox is always valid
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return true;
  }

  public void setEnabled( final boolean enabled )
  {
    m_checkbox.setEnabled( enabled );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    if( m_checkbox == null || m_checkbox.isDisposed() )
      return;

    final Feature feature = getFeature();
    if( feature != null && getFeatureTypeProperty() != null )
    {
      // compare with old to prevent loop
      final boolean oldValue = m_checkbox.getSelection();
      final Boolean newvalue = (Boolean)m_modifier.getValue( feature );
      if( newvalue.booleanValue() != oldValue )
        m_checkbox.setSelection( newvalue.booleanValue() );
    }
  }

  protected FeatureChange getChange()
  {
    final Feature feature = getFeature();

    final Boolean value = Boolean.valueOf( m_checkbox.getSelection() );

    final Object newData = m_modifier.parseInput( getFeature(), value );

    final String name = getFeatureTypeProperty().getName();
    final Object oldData = feature.getProperty( name );

    // nur ändern, wenn sich wirklich was geändert hat
    if( ( newData == null && oldData != null ) || ( newData != null && !newData.equals( oldData ) ) )
      return new FeatureChange( feature, name, newData );

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modlistener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modlistener.remove( l );
  }

  protected void fireModified()
  {
    for( final Iterator modIt = m_modlistener.iterator(); modIt.hasNext(); )
    {
      final ModifyListener l = (ModifyListener)modIt.next();
      l.modifyText( null );
    }
  }
}