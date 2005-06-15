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
import java.util.LinkedList;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.dialog.CalendarFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.FeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.RangeSetFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.RectifiedGridDomainFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.TimeserieLinkFeatureDialog;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.extension.ITypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.DateWithoutTime;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

/**
 * @author belger
 */
public class ButtonFeatureControl extends AbstractFeatureControl implements
    ModellEventListener
{
  private Button m_button;

  private IFeatureDialog m_dialog;

  private Collection m_modifyListener = new ArrayList();

  public ButtonFeatureControl( final GMLWorkspace workspace,
      final Feature feature, final FeatureTypeProperty ftp )
  {
    super( workspace, feature, ftp );

    m_dialog = chooseDialog( workspace, feature, ftp );
  }

  public static IFeatureDialog chooseDialog( final GMLWorkspace workspace,
      final Feature feature, final FeatureTypeProperty ftp )
  {
    final String typename = ftp.getType();
    // TODO make extensionpoint for this
    //    if( typename.equals( "java.lang.String" ) )
    //      return text;
    //    if( typeName.equals( "java.lang.Double" ) )
    //      return new Double( text );
    //    if( typeName.equals( "java.lang.Integer" ) )
    //      return new Integer( text );
    //    if( typeName.equals( "java.lang.Float" ) )
    //      return new Float( text );
    //    if( typeName.equals( "java.lang.Long" ) )
    //      return new Long( text );
    //    if( typeName.equals( "java.lang.Boolean" ) )
    //      return new Boolean( text );
    //    if( typeName.equals( "java.util.Date" ) )
    //      return DATE_FORMATTER.parse( text );
    if( DateWithoutTime.class.getName().equals( typename ) )
      return new CalendarFeatureDialog( feature, ftp );

    if( TypeRegistrySingleton.getTypeRegistry().hasClassName( typename ) )
    {
      final ITypeHandler handler = TypeRegistrySingleton.getTypeRegistry()
          .getTypeHandlerForClassName( typename );

      // TODO: TypeHandler should decide, what todo
      if( handler instanceof ObservationLinkHandler )
        return new TimeserieLinkFeatureDialog( workspace, feature, ftp );
    }

    if( "FeatureAssociationType".equals( typename ) )
    {
      int maxOccurs = feature.getFeatureType().getMaxOccurs( ftp.getName() );
      if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
      { // it is a list of features or links to features or mixed
        return new FeatureDialog( workspace, feature, ftp );
      }
      // it is not a list
      final Object property = feature.getProperty( ftp.getName() );
      final Feature linkedFeature;
      if( property instanceof String ) // link auf ein Feature mit FeatureID
      {
        if(((String)property).length()<1)
          return new NotImplementedFeatureDialog("hier ist kein Element verknüpft","<leer>");
        linkedFeature = workspace.getFeature( (String)property );
      }
      else if( property instanceof Feature )
      {
        linkedFeature = (Feature)property;
      }
      else
        return new NotImplementedFeatureDialog("hier ist kein Element verknüpft","<leer>");        
      return new FeatureDialog( workspace, linkedFeature );
    }

    if( RectifiedGridDomain.class.getName().equals( typename ) )
      return new RectifiedGridDomainFeatureDialog( feature, ftp );

    if( RangeSet.class.getName().equals( typename ) )
      return new RangeSetFeatureDialog( feature, ftp );

    return new NotImplementedFeatureDialog();
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    if( !( m_button.isDisposed() ) )
      m_button.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );
    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        buttonPressed();
      }
    } );

    updateControl();

    return m_button;
  }

  protected void buttonPressed()
  {
    if( m_dialog.open( m_button.getShell() ) == Window.OK )
    {
      final Collection c = new LinkedList();
      m_dialog.collectChanges( c );
      for( final Iterator iter = c.iterator(); iter.hasNext(); )
        fireChange( (FeatureChange)iter.next() );

      fireModfied();

      updateControl();
    }
  }

  private void fireModfied()
  {
    for( final Iterator iter = m_modifyListener.iterator(); iter.hasNext(); )
    {
      final ModifyListener l = (ModifyListener)iter.next();
      final Event event = new Event();
      // TODO: create a real event?
      event.widget = m_button;
      l.modifyText( new ModifyEvent( event ) );
    }
  }

  /**
   * Die ButtonControl ist immer valid
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    m_dialog.collectChanges( c );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modifyListener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modifyListener.remove( l );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_button.setText( m_dialog.getLabel() );
  }
}