/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.wizards.lengthsection;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.KalypsoFeatureThemeHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * 
 * properties page:
 * <P>
 * choose existing river line theme (line theme), river name field selection combo, river name selection combo, delta
 * station spinner, station field select combos (from / to). *
 * 
 * @author Thomas Jung * *
 */
public class ConfigureLengthSectionWizardPage extends WizardPage implements IWizardPage
{

  private static final String RIVER_NAME_FIELD = "NAME";

  private static final String FROM_STATION_FIELD = "RIVER_A";

  private static final String TO_STATION_FIELD = "RIVER_B";

  private IKalypsoFeatureTheme m_riverLineTheme;

  private IKalypsoFeatureTheme[] m_lineThemes;

  private final MapPanel m_mapPanel;

  private ComboViewer m_comboRiverLineNameField;

  private ComboViewer m_comboRiverLineName;

  private ComboViewer m_comboStationFromField;

  private ComboViewer m_comboStationToField;

  private final List<String> m_fieldNameList = new LinkedList<String>();

  private String m_riverNameField;

  private String m_toStationField;

  private String m_fromStationField;

  private final Set<Object> m_riverNameSet = new HashSet<Object>();

  protected int m_stationWidth;

  private FeatureList m_riverFeatures;

  private IPropertyType m_propertyType;

  public ConfigureLengthSectionWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final MapPanel mapPanel )
  {
    super( pageName, title, titleImage );
    m_mapPanel = mapPanel;
    m_stationWidth = 100;

    setDescription( "Wählen Sie auf dieser Seite die Gewässerachse aus." );

    m_lineThemes = KalypsoFeatureThemeHelper.getLineThemes( m_mapPanel );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final int comboWidth1 = 75;
    final int comboWidth2 = 50;

    /* set a fixed size to the Wizard */
    // final Object layoutData = parent.getLayoutData();
    // if( layoutData instanceof GridData )
    // {
    // final GridData pLayout = (GridData) layoutData;
    // pLayout.widthHint = 400;
    // pLayout.heightHint = 300;
    // parent.layout();
    // }
    final Composite composite = new Composite( parent, SWT.NONE );
    // composite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    composite.setLayout( new GridLayout() );

    /* river line selection group */
    final Group riverLineGroup = new Group( composite, SWT.NONE );
    riverLineGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineGroup.setLayout( new GridLayout( 2, false ) );
    riverLineGroup.setText( "Gewässerachse" );

    final Label riverLineText = new Label( riverLineGroup, SWT.NONE );
    riverLineText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineText.setText( "Bitte Thema der Gewässerachse auswählen" );

    final ComboViewer comboRiverLine = new ComboViewer( riverLineGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboRiverLine = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDatacomboRiverLine.widthHint = comboWidth1;
    comboRiverLine.getControl().setLayoutData( gridDatacomboRiverLine );
    comboRiverLine.setContentProvider( new ArrayContentProvider() );
    comboRiverLine.setLabelProvider( new LabelProvider() );

    comboRiverLine.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( selection.getFirstElement() instanceof IKalypsoFeatureTheme )
          setRiverLine( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* TODO: read the stored preferences */
    // final IKalypsoFeatureTheme riverTheme = m_preferences.getRiverTheme;
    m_lineThemes = KalypsoFeatureThemeHelper.getLineThemes( m_mapPanel );

    if( m_lineThemes == null )
      return;

    /*
     * define properties page ( river name field selection combo river name selection combo, delta station spinner,
     * station field select combo)
     */

    final Group propertyGroup = new Group( composite, SWT.NONE );
    propertyGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    propertyGroup.setLayout( new GridLayout( 2, false ) );
    propertyGroup.setText( "Parameter" );

    final Label riverLineNameFieldText = new Label( propertyGroup, SWT.NONE );
    riverLineNameFieldText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineNameFieldText.setText( "Spalte der Gewässer-ID auswählen" );

    m_comboRiverLineNameField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboRiverLineNameField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboRiverLineNameField.widthHint = comboWidth2;
    m_comboRiverLineNameField.getControl().setLayoutData( gridDatacomboRiverLineNameField );
    m_comboRiverLineNameField.setContentProvider( new ArrayContentProvider() );
    m_comboRiverLineNameField.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IPropertyType propertyType = (IPropertyType) element;
        return propertyType.getQName().getLocalPart();
      }
    } );

    m_comboRiverLineNameField.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        // get field names for river name field and station fields
        if( selection.getFirstElement() instanceof IPropertyType )
        {
          final IPropertyType property = (IPropertyType) selection.getFirstElement();
          m_riverNameField = property.getQName().getLocalPart();
          updateRiverNames();
        }
      }
    } );

    final Label riverLineNameText = new Label( propertyGroup, SWT.NONE );
    riverLineNameText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineNameText.setText( "Gewässer auswählen" );

    m_comboRiverLineName = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboRiverLineName = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboRiverLineName.widthHint = comboWidth2;
    m_comboRiverLineName.getControl().setLayoutData( gridDatacomboRiverLineName );
    m_comboRiverLineName.setContentProvider( new ArrayContentProvider() );

    final Label stationFromFieldText = new Label( propertyGroup, SWT.NONE );
    stationFromFieldText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stationFromFieldText.setText( "Spalte der Anfangsstation" );

    m_comboStationFromField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboStationFromField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboStationFromField.widthHint = comboWidth2;
    m_comboStationFromField.getControl().setLayoutData( gridDatacomboStationFromField );
    m_comboStationFromField.setContentProvider( new ArrayContentProvider() );
    m_comboStationFromField.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IPropertyType propertyType = (IPropertyType) element;
        return propertyType.getQName().getLocalPart();
      }
    } );

    final Label stationToFieldText = new Label( propertyGroup, SWT.NONE );
    stationToFieldText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stationToFieldText.setText( "Spalte der Endstation" );

    m_comboStationToField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboStationToField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboStationToField.widthHint = comboWidth2;
    m_comboStationToField.getControl().setLayoutData( gridDatacomboStationToField );
    m_comboStationToField.setContentProvider( new ArrayContentProvider() );
    m_comboStationToField.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IPropertyType propertyType = (IPropertyType) element;
        return propertyType.getQName().getLocalPart();
      }
    } );

    final Label stationSpinnerText = new Label( propertyGroup, SWT.NONE );
    stationSpinnerText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stationSpinnerText.setText( "Stützpunktweite des Längsschnitts [m]" );

    final Spinner stationSpinner = new Spinner( propertyGroup, SWT.BORDER | SWT.TRAIL );
    final GridData gridDatastationSpinner = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatastationSpinner.widthHint = 30;
    stationSpinner.setLayoutData( gridDatastationSpinner );

    final BigDecimal selectionValue = new BigDecimal( m_stationWidth ).setScale( 0, BigDecimal.ROUND_HALF_UP );
    stationSpinner.setValues( selectionValue.intValue(), 10, 1000, 0, 10, 100 );

    stationSpinner.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_stationWidth = stationSpinner.getSelection();
      }
    } );

    if( m_lineThemes.length == 0 )
    {
      comboRiverLine.getControl().setEnabled( false );
      m_comboRiverLineName.getControl().setEnabled( false );
      m_comboRiverLineNameField.getControl().setEnabled( false );
      m_comboStationFromField.getControl().setEnabled( false );
      m_comboStationToField.getControl().setEnabled( false );

      final String msg = "Kein Linienthema in Karte vorhanden.";
      comboRiverLine.setInput( new String[] { msg } );
      comboRiverLine.setSelection( new StructuredSelection( msg ) );
      m_riverLineTheme = null;

    }
    else
    {
      comboRiverLine.getControl().setEnabled( true );
      m_comboRiverLineName.getControl().setEnabled( true );
      m_comboRiverLineNameField.getControl().setEnabled( true );
      m_comboStationFromField.getControl().setEnabled( true );
      m_comboStationToField.getControl().setEnabled( true );

      comboRiverLine.setInput( m_lineThemes );

      // if( bankTheme != null )
      // m_riverLineTheme = bankTheme;
      // else
      m_riverLineTheme = m_lineThemes[0];

      comboRiverLine.setSelection( new StructuredSelection( m_riverLineTheme ) );
      setRiverLine( m_riverLineTheme );
    }

    setControl( composite );
  }

  protected void setRiverLine( final IKalypsoFeatureTheme theme )
  {
    if( m_comboRiverLineNameField == null )
      return;

    m_riverLineTheme = theme;

    m_fieldNameList.clear();

    // get Features
    m_riverFeatures = theme.getFeatureList();
    final IFeatureType targetFeatureType = m_riverFeatures.getParentFeatureTypeProperty().getTargetFeatureType();

    // get field names for river name field and station fields
    final IPropertyType[] properties = targetFeatureType.getProperties();

    final Object object = m_riverFeatures.first();
    final Feature feature = (Feature) object;

    m_comboRiverLineNameField.setInput( properties );
    // set river name field to "NAME". If it does not exist, set it to the first field
    final String customNamespace = feature.getFeatureType().getGMLSchema().getTargetNamespace(); // TODO: shape api?
    if( feature.getFeatureType().getProperty( new QName( customNamespace, RIVER_NAME_FIELD ) ) != null )
    {
      m_comboRiverLineNameField.setSelection( new StructuredSelection( feature.getFeatureType().getProperty( new QName( customNamespace, RIVER_NAME_FIELD ) ) ) );
      m_riverNameField = RIVER_NAME_FIELD;
    }
    else
    {
      final Object elementAt = m_comboRiverLineName.getElementAt( 1 );
      if( elementAt != null )
      {
        m_comboRiverLineNameField.setSelection( new StructuredSelection( elementAt ) );
        m_riverNameField = properties[0].getQName().getLocalPart();
      }
    }

    if( m_riverNameField == null )
    {
      MessageDialog.openInformation( getShell(), "Linienthema auswählen", "Kein geeignetes Linienthema ausgewählt.");
      return;
    }

    // set the station fields to "RIVER_A" and "RIVER_B", If they does not exist, set them to the first field

    m_comboStationFromField.setInput( properties );
    // set river name field to "NAME". If it does not exist, set it to the first field
    if( feature.getFeatureType().getProperty( new QName( customNamespace, FROM_STATION_FIELD ) ) != null )
    {
      m_comboStationFromField.setSelection( new StructuredSelection( feature.getFeatureType().getProperty( new QName( customNamespace, FROM_STATION_FIELD ) ) ) );
      m_fromStationField = FROM_STATION_FIELD;
    }
    else
    {
      m_comboStationFromField.setSelection( new StructuredSelection( m_comboStationFromField.getElementAt( 1 ) ) );
      m_fromStationField = properties[0].getQName().getLocalPart();
    }

    m_comboStationToField.setInput( properties );
    // set river name field to "NAME". If it does not exist, set it to the first field
    if( feature.getFeatureType().getProperty( new QName( customNamespace, TO_STATION_FIELD ) ) != null )
    {
      m_comboStationToField.setSelection( new StructuredSelection( feature.getFeatureType().getProperty( new QName( customNamespace, TO_STATION_FIELD ) ) ) );
      m_toStationField = TO_STATION_FIELD;
    }
    else
    {
      m_comboStationToField.setSelection( new StructuredSelection( m_comboStationToField.getElementAt( 1 ) ) );
      m_toStationField = properties[0].getQName().getLocalPart();
    }

    // get all river names

    m_propertyType = feature.getFeatureType().getProperty( new QName( customNamespace, m_riverNameField ) );
    final ITypeRegistry<IGuiTypeHandler> typeRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    final IGuiTypeHandler guiTypeHandler = typeRegistry.getTypeHandlerFor( m_propertyType );

    updateRiverNames();

    if( m_comboRiverLineName != null )
    {
      m_comboRiverLineName.setLabelProvider( new LabelProvider()
      {
        /**
         * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
         */
        @Override
        public String getText( final Object element )
        {
          return guiTypeHandler.getText( element );
        }
      } );
    }
  }

  private void updateRiverNames( )
  {
    if( m_riverNameField == null )
      return;

    m_riverNameSet.clear();

    m_riverFeatures = m_riverLineTheme.getFeatureList();
    IPropertyType propertyType;
    for( final Object o : m_riverFeatures )
    {
      final Feature riverFeature = (Feature) o;
      propertyType = riverFeature.getFeatureType().getProperty( new QName( "namespace", m_riverNameField ) );

      if( propertyType instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) propertyType;
        if( vpt.getValueClass() == String.class || vpt.getValueClass() == Double.class || vpt.getValueClass() == Integer.class || vpt.getValueClass() == Long.class )
        {
          m_riverNameSet.add( riverFeature.getProperty( propertyType ) );
        }
      }
    }
    m_comboRiverLineName.setInput( m_riverNameSet );
    final Object element = m_comboRiverLineName.getElementAt( 0 );
    if( element == null )
    {
      MessageDialog.openInformation( getShell(), "Gewässerfeld auswählen", "Ausgewähltes Feld enthält keine geeigneten Informationen." );
      return;
    }
    m_comboRiverLineName.setSelection( new StructuredSelection( element ) );
  }

  public LengthSectionParameters getLengthSectionParameters( )
  {
    final IStructuredSelection selRiverName = (IStructuredSelection) m_comboRiverLineNameField.getSelection();
    final IStructuredSelection selFrom = (IStructuredSelection) m_comboStationFromField.getSelection();
    final IStructuredSelection selTo = (IStructuredSelection) m_comboStationToField.getSelection();

    final IPropertyType riverNamePropertyType = (IPropertyType) selRiverName.getFirstElement();
    final IPropertyType fromStationPropertyType = (IPropertyType) selFrom.getFirstElement();
    final IPropertyType toStationPropertyType = (IPropertyType) selTo.getFirstElement();

    return new LengthSectionParameters( m_riverFeatures, riverNamePropertyType, fromStationPropertyType, toStationPropertyType, new BigDecimal( m_stationWidth ) );
  }

}
