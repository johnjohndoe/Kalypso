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
package org.kalypso.ui.rrm.internal.gml;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.ButtonModifier;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.view.observationDialog.ClipboardExportAction;
import org.kalypso.ogc.sensor.view.observationDialog.ClipboardImportAction;
import org.kalypso.ogc.sensor.view.observationDialog.IObservationAction;
import org.kalypso.ogc.sensor.view.observationDialog.RemoveObservationAction;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author kuepfer
 */
public class ZmlInlineGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  private final ZmlInlineTypeHandler m_typeHandler;

  public ZmlInlineGuiTypeHandler( final ZmlInlineTypeHandler typeHandler )
  {
    m_typeHandler = typeHandler;
  }

  protected ZmlInlineTypeHandler getTypeHandler( )
  {
    return m_typeHandler;
  }

  @Override
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new ZmlInlineFeatureDialog( feature, ftp, this );
  }

  @Override
  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    final Button button = factory.createButton();
    button.setStyle( "SWT.PUSH" ); //$NON-NLS-1$
    button.setProperty( property.getQName() );

    return factory.createButton( button );
  }

  @Override
  public IFeatureModifier createFeatureModifier( final GMLXPath propertyPath, final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl, final String format )
  {
    return new ButtonModifier( propertyPath, ftp, fcl );
  }

  @Override
  public Class< ? > getValueClass( )
  {
    return m_typeHandler.getValueClass();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  @Override
  public QName getTypeName( )
  {
    return m_typeHandler.getTypeName();
  }

  @Override
  public String getText( final Object o )
  {
    return Messages.getString( "org.kalypso.ogc.gml.gui.ZmlInlineGuiTypeHandler.1" ); //$NON-NLS-1$
  }

  @Override
  public boolean isGeometry( )
  {
    return false;
  }

  @Override
  public Object parseText( final String text, final String formatHint ) throws ParseException
  {
    // Standard is to use the parseType method from the corresponding marhsalling type handler
    // In future, this should be directly implemented at this point
    final IMarshallingTypeHandler marshallingHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( getTypeName() );
    return marshallingHandler.parseType( text );
  }

  public IObservationAction[] getObservationActions( )
  {
    final Collection<IObservationAction> actions = new ArrayList<>();

    actions.add( getNewObservationAction() );
    actions.add( new RemoveObservationAction() );
    actions.add( new ClipboardImportAction( m_typeHandler ) );
    actions.add( new ClipboardExportAction() );

    return actions.toArray( new IObservationAction[actions.size()] );
  }

  protected IObservationAction getNewObservationAction( )
  {
    return new NewObservationAction( m_typeHandler );
  }

  /**
   * Allow the type handler to check and possibly fix/convert the given observation.<br/>
   * The default implementation copies the timeseries and implicitely makes sure that exactly the given axes are
   * present. <br/>
   * This also fixes the bug, that cancel did not work on the observation dialog.
   */
  public IObservation checkObservation( final IObservation o ) throws SensorException
  {
    if( o == null )
      return null;

    final ITupleModel model = o.getValues( null );

    final IAxis[] newAxis = m_typeHandler.createAxes();

    final CopyObservationVisitor visitor = new CopyObservationVisitor( newAxis );
    final Map<String, String> nameMapping = createCheckObservationNameMapping();
    visitor.setNameMapping( nameMapping );
    model.accept( visitor, 1 );

    /* copy the rest */
    final String href = o.getHref();
    final String name = o.getName();
    final MetadataList metadata = (MetadataList) o.getMetadataList().clone();

    final Object[][] values = visitor.getValues();
    final SimpleTupleModel newModel = new SimpleTupleModel( newAxis, values );
    return new SimpleObservation( href, name, metadata, newModel );
  }

  /**
   * Additional name mapping that allows to read and check old time series.<br/>
   * The mapping maps old axis-names to new ones.<br/>
   * The default implementation returns the empty map.
   */
  protected Map<String, String> createCheckObservationNameMapping( )
  {
    return Collections.emptyMap();
  }
}