package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import java.util.ArrayList;
import javax.swing.event.EventListenerList;
import org.deegree.graphics.sld.Rule;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.services.wfs.filterencoding.Expression;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterConstructionException;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.AbstractFilter;
import org.deegree_impl.services.wfs.filterencoding.BoundaryExpression;
import org.deegree_impl.services.wfs.filterencoding.ComparisonOperation;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.LogicalOperation;
import org.deegree_impl.services.wfs.filterencoding.OperationDefines;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsCOMPOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsLikeOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsNullOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.editor.styleeditor.dialogs.filterencoding.ElseFilter;
import org.kalypso.ui.editor.styleeditor.panels.ComparisonFilterComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.LogicalFilterComboPanel;

public class FilterDialog extends Dialog implements ISelectionChangedListener
{

  private TableTreeViewer m_viewer;

  private FilterDialogTreeNode mRoot = null;

  private LogicalFilterComboPanel logicalCombo = null;

  private Button logicalButton = null;

  private ComparisonFilterComboPanel comparisonCombo = null;

  private Button compButton = null;

  private Button featureIdButton = null;

  private FeatureType featureType = null;

  private Group configureGroup = null;

  private FilterDialogTreeNode currentNode = mRoot;

  private Button elseFilterButton = null;

  private Composite globalConfigureComposite = null;

  private Composite innerConfigureComposite = null;

  private Button validateFilterButton = null;

  private boolean isValidated = false;

  private Filter returnFilter = null;

  private Rule rule = null;

  private Label errorLabel = null;

  private EventListenerList listenerList = new EventListenerList();

  private Rule historyRule = null;

  public FilterDialog( Shell parent, FeatureType m_featureType, Rule m_rule )
  {
    super( parent );
    setFeatureType( m_featureType );
    setRule( m_rule );
    Filter filter = rule.getFilter();
    if( filter != null && filter instanceof AbstractFilter )
    {
      FilterDialogTreeNode filterTree = parseFilterIntoTree( (AbstractFilter)filter );
      if( filterTree != null )
      {
        mRoot = new FilterDialogTreeNode(
            "Filter                                                              ",
            FilterDialogTreeNode.ROOT_TYPE );
        mRoot.getParent().addNode( filterTree );
      }
    }
    historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(), rule.getTitle(),
        rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule.hasElseFilter(), rule
            .getMinScaleDenominator(), rule.getMaxScaleDenominator() );
  }

  protected void configureShell( Shell shell )
  {
    super.configureShell( shell );
    shell.setText( "Filter Dialog" );
    shell.setSize( 500, 450 );
  }

  protected void okPressed()
  {
    Object[] children = ( (FilterDialogTreeNode)mRoot.getChildren()[0] ).getChildren();
    if( children.length != 0 )
    {
      boolean validFilter = false;
      try
      {
        validFilter = validateFilter( children );
      }
      catch( FilterDialogException e1 )
      {
        m_viewer.setSelection( new StructuredSelection( e1.getError().getNode() ) );
        m_viewer.expandAll();
        errorLabel.setText( e1.getError().getFaultCode() );
      }
      if( validFilter )
      {
        returnFilter = generateFilter( children );
        if( returnFilter instanceof ElseFilter )
        {
          rule.setElseFilter( true );
          rule.setFilter( null );
        }
        else
        {
          rule.setFilter( getFilter() );
          rule.setElseFilter( false );
        }
        historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(), rule
            .getTitle(), rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule
            .hasElseFilter(), rule.getMinScaleDenominator(), rule.getMaxScaleDenominator() );
        fire();
        super.okPressed();
      }
    }
    // no filter -> set empty filter
    else
    {
      returnFilter = null;
      rule.setFilter( null );
      rule.setElseFilter( false );
      historyRule = StyleFactory.createRule( rule.getSymbolizers(), rule.getName(),
          rule.getTitle(), rule.getAbstract(), rule.getLegendGraphic(), rule.getFilter(), rule
              .hasElseFilter(), rule.getMinScaleDenominator(), rule.getMaxScaleDenominator() );
      fire();
      super.okPressed();
    }
  }

  protected void cancelPressed()
  {
    returnFilter = historyRule.getFilter();
    rule.setFilter( historyRule.getFilter() );
    rule.setElseFilter( historyRule.hasElseFilter() );
    FilterDialogTreeNode filterTree = parseFilterIntoTree( (AbstractFilter)historyRule.getFilter() );
    if( filterTree != null )
    {
      mRoot = new FilterDialogTreeNode(
          "Filter                                                              ",
          FilterDialogTreeNode.ROOT_TYPE );
      mRoot.getParent().addNode( filterTree );
    }
    fire();
    super.cancelPressed();
  }

  public void addFilterDialogListener( FilterDialogListener pl )
  {
    listenerList.add( FilterDialogListener.class, pl );
  }

  protected Control createDialogArea( Composite parent )
  {
    returnFilter = null;
    Composite composite = (Composite)super.createDialogArea( parent );
    composite.setSize( 500, 300 );
    composite.setLayout( new GridLayout( 1, true ) );
    composite.layout();
    applyDialogFont( composite );

    // **** TITLE/NAME of rule
    Label nameLabel = new Label( composite, 0 );
    if( rule.getTitle() != null )
      nameLabel.setText( "Filter for Rule: " + rule.getTitle() );
    else
      nameLabel.setText( "Filter for Rule: " + rule.getName() );
    nameLabel.setFont( new Font( null, "Arial", 10, SWT.BOLD ) );

    // ****
    Label titleLabel = new Label( composite, SWT.NULL );
    titleLabel.setText( "Available Filter types" );
    titleLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData titleLabelData = new GridData();
    titleLabelData.horizontalAlignment = GridData.CENTER;
    titleLabelData.verticalAlignment = GridData.END;
    titleLabel.setLayoutData( titleLabelData );

    // **** THIRD ROW - FUNCTION MENU
    Composite functionComposite = new Composite( composite, SWT.NULL );
    GridData ogcGroupData = new GridData();
    ogcGroupData.widthHint = 235;
    ogcGroupData.heightHint = 100;
    functionComposite.setLayout( new GridLayout( 2, false ) );
    Group ogcFilter = new Group( functionComposite, SWT.NULL );
    ogcFilter.setText( "OGC-Filter" );
    ogcFilter.setLayoutData( ogcGroupData );
    Group sldFilter = new Group( functionComposite, SWT.NULL );
    sldFilter.setText( "SLD-Filter" );
    GridData sldGroupData = new GridData();
    sldGroupData.widthHint = 210;
    sldGroupData.heightHint = 100;
    sldFilter.setLayoutData( sldGroupData );

    // ----- OGC Filter
    ogcFilter.setLayout( new GridLayout( 3, false ) );
    // ++++ Logical filter line
    Label logicalLabel = new Label( ogcFilter, SWT.NULL );
    logicalLabel.setText( "Logical:" );
    logicalCombo = new LogicalFilterComboPanel( ogcFilter );
    logicalButton = new Button( ogcFilter, SWT.NULL );
    logicalButton.setText( "Add" );
    logicalButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( getLogicalCombo()
            .getSelectionName( getLogicalCombo().getSelection() ),
            FilterDialogTreeNode.LOGICAL_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( childNode != null )
          getM_viewer().setSelection( new StructuredSelection( childNode ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    // ++++ Comparison filter line
    Label comparisonLabel = new Label( ogcFilter, SWT.NULL );
    comparisonLabel.setText( "Comparison:" );
    comparisonCombo = new ComparisonFilterComboPanel( ogcFilter );
    compButton = new Button( ogcFilter, SWT.NULL );
    compButton.setText( "Add" );
    compButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( getComparisonCombo()
            .getSelectionName( getComparisonCombo().getSelection() ),
            FilterDialogTreeNode.COMPARISON_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );
    //	++++ Comparison FilterFilter line
    Label featureFilterLabel = new Label( ogcFilter, SWT.NULL );
    featureFilterLabel.setText( "FeatureFilter:" );
    featureIdButton = new Button( ogcFilter, SWT.NULL );
    featureIdButton.setText( "Add FeatureFilter" );
    featureIdButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( "FT_ID",
            FilterDialogTreeNode.FEATUREID_NODE_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    // ----- SLD Filter
    sldFilter.setLayout( new GridLayout( 2, false ) );
    //	++++ Comparison ElseFilter line
    Label elseFilterLabel = new Label( sldFilter, SWT.NULL );
    elseFilterLabel.setText( "ElseFilter:" );
    elseFilterButton = new Button( sldFilter, SWT.NULL );
    elseFilterButton.setText( "Add ElseFilter" );
    elseFilterButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FilterDialogTreeNode childNode = new FilterDialogTreeNode( "ElseFilter",
            FilterDialogTreeNode.ELSEFILTER_TYPE );
        getCurrentNode().addNode( childNode );
        getM_viewer().expandAll();
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().refresh( true );
        setFilterInvalid();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    // **** FOURTH ROW
    Composite secondRowComposite = new Composite( composite, SWT.NULL );
    secondRowComposite.setLayout( new GridLayout( 2, true ) );

    Label treeLabel = new Label( secondRowComposite, SWT.NULL );
    treeLabel.setText( "Filter-Structure" );
    treeLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData treeLabelData = new GridData();
    treeLabelData.horizontalAlignment = GridData.CENTER;
    treeLabelData.verticalAlignment = GridData.END;
    treeLabel.setLayoutData( treeLabelData );

    Label inputLabel = new Label( secondRowComposite, SWT.NULL );
    inputLabel.setText( "Configuration" );
    inputLabel.setFont( new Font( null, "Arial", 8, SWT.BOLD ) );
    GridData inputLabelData = new GridData();
    inputLabelData.horizontalAlignment = GridData.CENTER;
    inputLabelData.verticalAlignment = GridData.END;
    inputLabel.setLayoutData( inputLabelData );

    //  **** FIFTH ROW - TREE
    final TableTree tree = new TableTree( secondRowComposite, SWT.SINGLE | SWT.FULL_SELECTION
        | SWT.H_SCROLL | SWT.BORDER );
    GridData tableTreeData = new GridData();
    tableTreeData.widthHint = 224;
    tableTreeData.heightHint = 97;
    tree.setLayoutData( tableTreeData );
    m_viewer = new TableTreeViewer( tree );
    m_viewer.addSelectionChangedListener( this );
    m_viewer.setContentProvider( new FilterDialogTreeContentProvider() );
    m_viewer.setLabelProvider( new FilterDialogLabelProvider() );
    if( mRoot == null )
      mRoot = new FilterDialogTreeNode(
          "Filter                                                              ",
          FilterDialogTreeNode.ROOT_TYPE );
    m_viewer.setInput( mRoot );
    m_viewer.setSelection( new StructuredSelection( mRoot.getParent() ) );
    m_viewer.expandAll();
    createContextMenu( m_viewer.getControl() );

    globalConfigureComposite = new Composite( secondRowComposite, SWT.NULL );
    globalConfigureComposite.setLayout( new GridLayout() );
    GridData globalConfigureCompositeData = new GridData();
    globalConfigureCompositeData.widthHint = 224;
    globalConfigureCompositeData.heightHint = 127;
    globalConfigureComposite.setLayoutData( globalConfigureCompositeData );

    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );

    // **** THIRD ROW - SAVE FILTER
    Composite thirdRowComposite = new Composite( composite, SWT.NULL );
    thirdRowComposite.setLayout( new GridLayout( 2, false ) );
    validateFilterButton = new Button( thirdRowComposite, SWT.NULL );
    validateFilterButton.setText( "validate" );
    validateFilterButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        Object[] children = ( (FilterDialogTreeNode)getMRoot().getChildren()[0] ).getChildren();
        if( children.length != 0 )
        {
          // generate Filter
          if( isValidated() )
          {
            getErrorLabel().setText( "" );
            getValidateFilterButton().setText( "apply" );
            setReturnFilter( generateFilter( children ) );
            if( getReturnFilter() instanceof ElseFilter )
            {
              getRule().setElseFilter( true );
              getRule().setFilter( null );
            }
            else
            {
              getRule().setFilter( getFilter() );
              getRule().setElseFilter( false );
            }
            fire();
          }
          else
          {
            boolean validFilter = false;
            try
            {
              validFilter = validateFilter( children );
            }
            catch( FilterDialogException e1 )
            {
              getM_viewer().setSelection( new StructuredSelection( e1.getError().getNode() ) );
              getM_viewer().expandAll();
              getErrorLabel().setText( e1.getError().getFaultCode() );
            }

            if( validFilter )
            {
              getValidateFilterButton().setText( "apply" );
              setValidated( true );
            }
          }
        }
        // no filter -> set empty filter
        else
        {
          if( isValidated() )
          {
            getErrorLabel().setText( "" );
            getValidateFilterButton().setText( "apply" );
            setReturnFilter( null );
            getRule().setFilter( null );
            getRule().setElseFilter( false );
            fire();
          }
          else
          {
            getValidateFilterButton().setText( "apply" );
            setValidated( true );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    errorLabel = new Label( thirdRowComposite, SWT.NULL );
    GridData errorLabelData = new GridData();
    errorLabelData.widthHint = 350;
    errorLabel.setLayoutData( errorLabelData );

    return composite;
  }

  private FilterDialogTreeNode parseFilterIntoTree( AbstractFilter filter )
  {
    if( filter instanceof FeatureFilter )
    {
      FeatureFilter featureFilter = (FeatureFilter)filter;
      ArrayList featureIds = featureFilter.getFeatureIds();
      FilterDialogTreeNode tmpNode = null;
      FeatureIDData tmpData = null;
      for( int i = 0; i < featureIds.size(); i++ )
      {
        tmpNode = new FilterDialogTreeNode( "FT_ID", FilterDialogTreeNode.FEATUREID_NODE_TYPE );
        tmpData = new FeatureIDData();
        tmpData.setFeatureId( featureIds.get( i ).toString() );
        tmpNode.setData( tmpData );
      }
      return tmpNode;
    }
    else if( filter instanceof ComplexFilter )
    {
      ComplexFilter complexFilter = (ComplexFilter)filter;
      Operation operation = complexFilter.getOperation();
      if( operation instanceof LogicalOperation )
      {
        LogicalOperation logOp = (LogicalOperation)operation;
        FilterDialogTreeNode tmpNode = null;
        switch( logOp.getOperatorId() )
        {
        case OperationDefines.AND:
        {
          tmpNode = new FilterDialogTreeNode( "AND", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
          break;
        }
        case OperationDefines.OR:
        {
          tmpNode = new FilterDialogTreeNode( "OR", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
          break;
        }
        case OperationDefines.NOT:
        {
          tmpNode = new FilterDialogTreeNode( "NOT", FilterDialogTreeNode.LOGICAL_NODE_TYPE );
          break;
        }
        }
        // ArrayList with Operations (logic, comp, spatial)
        ArrayList arguments = logOp.getArguments();
        for( int i = 0; i < arguments.size(); i++ )
        {
          ComplexFilter tmpFilter = new ComplexFilter( (Operation)arguments.get( i ) );
          tmpNode.addNode( parseFilterIntoTree( tmpFilter ) );
        }
        return tmpNode;
      }
      else if( operation instanceof ComparisonOperation )
      {
        ComparisonOperation compOp = (ComparisonOperation)operation;
        FilterDialogTreeNode tmpNode = null;
        switch( compOp.getOperatorId() )
        {
        case OperationDefines.PROPERTYISLIKE:
        {
          tmpNode = new FilterDialogTreeNode( "LIKE", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsLikeOperation isLikeOp = (PropertyIsLikeOperation)compOp;
          LikeComparisonData data = new LikeComparisonData();
          data.setLiteral( isLikeOp.getLiteral().getValue() );
          data.setPropertyName( isLikeOp.getPropertyName().getValue() );
          data.setEscapeChar( isLikeOp.getEscapeChar() );
          data.setSingleChar( isLikeOp.getSingleChar() );
          data.setWildCard( isLikeOp.getWildCard() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISNULL:
        {
          tmpNode = new FilterDialogTreeNode( "NULL", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsNullOperation isNullOp = (PropertyIsNullOperation)compOp;
          NullComparisonData data = new NullComparisonData();
          Expression exp = isNullOp.getExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISBETWEEN:
        {
          tmpNode = new FilterDialogTreeNode( "BETWEEN", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsBetweenOperation isBetweenOp = (PropertyIsBetweenOperation)compOp;
          BetweenComparisonData data = new BetweenComparisonData();
          data.setPropertyName( isBetweenOp.getPropertyName().getValue() );
          data.setLower( ( (BoundaryExpression)isBetweenOp.getLowerBoundary() ).getValue() );
          data.setUpper( ( (BoundaryExpression)isBetweenOp.getUpperBoundary() ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISEQUALTO:
        {
          tmpNode = new FilterDialogTreeNode( "EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation)compOp;
          BinaryComparisonData data = new BinaryComparisonData();
          Expression exp = isCompOp.getFirstExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          exp = isCompOp.getSecondExpression();
          if( exp instanceof Literal )
            data.setLiteral( ( (Literal)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISLESSTHAN:
        {
          tmpNode = new FilterDialogTreeNode( "LESS_THAN",
              FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation)compOp;
          BinaryComparisonData data = new BinaryComparisonData();
          Expression exp = isCompOp.getFirstExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          exp = isCompOp.getSecondExpression();
          if( exp instanceof Literal )
            data.setLiteral( ( (Literal)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISGREATERTHAN:
        {
          tmpNode = new FilterDialogTreeNode( "GREATER_THAN",
              FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation)compOp;
          BinaryComparisonData data = new BinaryComparisonData();
          Expression exp = isCompOp.getFirstExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          exp = isCompOp.getSecondExpression();
          if( exp instanceof Literal )
            data.setLiteral( ( (Literal)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
        {
          tmpNode = new FilterDialogTreeNode( "LESS_THAN_OR_EQUAL_TO",
              FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation)compOp;
          BinaryComparisonData data = new BinaryComparisonData();
          Expression exp = isCompOp.getFirstExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          exp = isCompOp.getSecondExpression();
          if( exp instanceof Literal )
            data.setLiteral( ( (Literal)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
        {
          tmpNode = new FilterDialogTreeNode( "GREATER_THAN_OR_EQUAL_TO",
              FilterDialogTreeNode.COMPARISON_NODE_TYPE );
          PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation)compOp;
          BinaryComparisonData data = new BinaryComparisonData();
          Expression exp = isCompOp.getFirstExpression();
          if( exp instanceof PropertyName )
            data.setPropertyName( ( (PropertyName)exp ).getValue() );
          exp = isCompOp.getSecondExpression();
          if( exp instanceof Literal )
            data.setLiteral( ( (Literal)exp ).getValue() );
          tmpNode.setData( data );
          break;
        }
        }
        return tmpNode;
      }
    }
    return null;
  }

  Filter generateFilter( Object[] children )
  {
    if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
    {
      FeatureFilter filter = new FeatureFilter();
      for( int i = 0; i < children.length; i++ )
      {
        FilterDialogTreeNode currentChild = (FilterDialogTreeNode)children[i];
        if( currentChild.getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
        {
          FeatureIDData data = (FeatureIDData)currentChild.getData();
          filter.addFeatureId( new FeatureId( data.getFeatureId() ) );
        }
      }
      return filter;
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.ELSEFILTER_TYPE )
    {
      return new ElseFilter();
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode)children[0];

      switch( child.getSubType() )
      {
      case FilterDialogTreeNode.COMPARISON_BETWEEN:
      {
        BetweenComparisonData data = (BetweenComparisonData)child.getData();
        BoundaryExpression upperBoundary = new BoundaryExpression( data.getUpper() );
        BoundaryExpression lowerBoundary = new BoundaryExpression( data.getLower() );
        PropertyName propertyName = new PropertyName( data.getPropertyName() );
        PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation( propertyName,
            lowerBoundary, upperBoundary );
        return new ComplexFilter( operation );
      }
      case FilterDialogTreeNode.COMPARISON_LIKE:
      {
        LikeComparisonData data = (LikeComparisonData)child.getData();
        PropertyName propertyName = new PropertyName( data.getPropertyName() );
        Literal literal = new Literal( data.getLiteral() );
        PropertyIsLikeOperation operation = new PropertyIsLikeOperation( propertyName, literal,
            data.getWildCard(), data.getSingleChar(), data.getEscapeChar() );
        return new ComplexFilter( operation );
      }
      case FilterDialogTreeNode.COMPARISON_NULL:
      {
        NullComparisonData data = (NullComparisonData)child.getData();
        PropertyName propertyName = new PropertyName( data.getPropertyName() );
        PropertyIsNullOperation operation = new PropertyIsNullOperation( propertyName );
        return new ComplexFilter( operation );
      }
      case FilterDialogTreeNode.COMPARISON_EQUALTO:
      case FilterDialogTreeNode.COMPARISON_LESSTHAN:
      case FilterDialogTreeNode.COMPARISON_GREATERTHAN:
      case FilterDialogTreeNode.COMPARISON_LESSTHANOREQUALTO:
      case FilterDialogTreeNode.COMPARISON_GREATERTHANOREQUALTO:
      {
        Literal literal = null;
        PropertyName propertyName = null;

        if( child.getData() instanceof BinaryComparisonData )
        {
          BinaryComparisonData data = (BinaryComparisonData)child.getData();
          propertyName = new PropertyName( data.getPropertyName() );
          literal = new Literal( data.getLiteral() );
        }
        else if( child.getData() instanceof BinaryComparisonNumericData )
        {
          BinaryComparisonNumericData data = (BinaryComparisonNumericData)child.getData();
          propertyName = new PropertyName( data.getPropertyName() );
          literal = new Literal( data.getLiteral() );
        }
        PropertyIsCOMPOperation operation = new PropertyIsCOMPOperation( child.getSubType(),
            propertyName, literal );
        return new ComplexFilter( operation );
      }
      }
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode)children[0];
      if( child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
      {
        ArrayList arguments = new ArrayList();
        Object[] innerElement =
        { ( (FilterDialogTreeNode)child.getChildren()[0] ) };
        Filter filter = generateFilter( innerElement );
        if( filter instanceof ComplexFilter )
        {
          arguments.add( ( (ComplexFilter)filter ).getOperation() );
        }
        try
        {
          LogicalOperation operation = new LogicalOperation( OperationDefines.NOT, arguments );
          return new ComplexFilter( operation );
        }
        catch( FilterConstructionException e )
        {
          e.printStackTrace();
        }
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_AND )
      {
        ArrayList arguments = new ArrayList();
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          Object[] innerElement =
          { ( (FilterDialogTreeNode)child.getChildren()[i] ) };
          Filter filter = generateFilter( innerElement );
          if( filter instanceof ComplexFilter )
          {
            arguments.add( ( (ComplexFilter)filter ).getOperation() );
          }
        }
        try
        {
          LogicalOperation operation = new LogicalOperation( OperationDefines.AND, arguments );
          return new ComplexFilter( operation );
        }
        catch( FilterConstructionException e )
        {
          e.printStackTrace();
        }
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_OR )
      {
        ArrayList arguments = new ArrayList();
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          Object[] innerElement =
          { ( (FilterDialogTreeNode)child.getChildren()[i] ) };
          Filter filter = generateFilter( innerElement );
          if( filter instanceof ComplexFilter )
          {
            arguments.add( ( (ComplexFilter)filter ).getOperation() );
          }
        }
        try
        {
          LogicalOperation operation = new LogicalOperation( OperationDefines.OR, arguments );
          return new ComplexFilter( operation );
        }
        catch( FilterConstructionException e )
        {
          e.printStackTrace();
        }
      }

    }
    return null;
  }

  // parses the Filter to check whether valid structure
  boolean validateFilter( Object[] children ) throws FilterDialogException
  {
    // element has to have at least one child
    if( children.length == 0 )
      return false;

    if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
    {
      // check every FeatureIDElement whether valid
      for( int i = 0; i < children.length; i++ )
      {
        FilterDialogTreeNode currentChild = (FilterDialogTreeNode)children[i];
        // if one is not valid
        if( !currentChild.validate() )
          return false;
      }
      return true;
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.ELSEFILTER_TYPE )
    {
      return true;
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE )
    {
      FilterDialogTreeNode currentChild = (FilterDialogTreeNode)children[0];
      if( !currentChild.validate() )
        return false;
      return true;
    }
    else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
    {
      FilterDialogTreeNode child = (FilterDialogTreeNode)children[0];
      if( child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
      {
        boolean evaluation = false;
        // NOT needs to have exactly one child
        if( child.getChildren().length != 1 )
          throw new FilterDialogException( new FilterDialogError( child, "needs to have one child" ) );

        if( ( (FilterDialogTreeNode)child.getChildren()[0] ).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
        {
          FilterDialogTreeNode[] tmpArray =
          { (FilterDialogTreeNode)child.getChildren()[0] };
          evaluation = validateFilter( tmpArray );
          if( !evaluation )
            throw new FilterDialogException( new FilterDialogError( (FilterDialogTreeNode)child
                .getChildren()[0], "has to have one child" ) );
        }
        else
        {
          if( !( (FilterDialogTreeNode)child.getChildren()[0] ).validate() )
            return false;
        }
        return true;
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_AND )
      {
        boolean evaluation = false;
        // AND needs to have more than 1 child
        if( child.getChildren().length < 2 )
          throw new FilterDialogException( new FilterDialogError( child,
              "needs to have at least two children" ) );
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          if( ( (FilterDialogTreeNode)child.getChildren()[i] ).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
          {
            FilterDialogTreeNode[] tmpArray =
            { (FilterDialogTreeNode)child.getChildren()[i] };
            evaluation = validateFilter( tmpArray );
            if( !evaluation )
              return false;
          }
          else if( !( (FilterDialogTreeNode)child.getChildren()[i] ).validate() )
            return false;
        }
        return true;
      }
      else if( child.getSubType() == FilterDialogTreeNode.LOCICAL_OR )
      {
        boolean evaluation = false;
        // OR needs to have more than 1 child
        if( child.getChildren().length < 2 )
          throw new FilterDialogException( new FilterDialogError( child,
              "needs to have at least two children" ) );
        for( int i = 0; i < child.getChildren().length; i++ )
        {
          if( ( (FilterDialogTreeNode)child.getChildren()[i] ).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE )
          {
            FilterDialogTreeNode[] tmpArray =
            { (FilterDialogTreeNode)child.getChildren()[i] };
            evaluation = validateFilter( tmpArray );
            if( !evaluation )
              return false;
          }
          else if( !( (FilterDialogTreeNode)child.getChildren()[i] ).validate() )
            return false;
        }
        return true;
      }
    }
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    TableTreeViewer ttv = (TableTreeViewer)event.getSource();
    IStructuredSelection s = (IStructuredSelection)ttv.getSelection();
    if( s.getFirstElement() instanceof FilterDialogTreeNode )
    {
      currentNode = (FilterDialogTreeNode)s.getFirstElement();
      Object[] children = currentNode.getChildren();
      if( globalConfigureComposite != null && !globalConfigureComposite.isDisposed() )
        globalConfigureComposite.setVisible( false );
      // always disabled -> only root enables it if nothing added yet
      disableElseFilter();

      switch( currentNode.getType() )
      {
      case FilterDialogTreeNode.ROOT_TYPE:
      {
        // no first element -> all options available
        if( children.length == 0 )
        {
          enableComparisonOperations();
          enableFeatureOperations();
          enableLogicalOperations();
          enableElseFilter();
        }
        // first element: featureId -> only featureId available
        else if( ( (FilterDialogTreeNode)children[0] ).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE )
        {
          disableComparisonOperations();
          enableFeatureOperations();
          disableLogicalOperations();
        }
        // first element: logic or comparision -> no options
        else
        {
          disableComparisonOperations();
          disableFeatureOperations();
          disableLogicalOperations();
        }
        break;
      }
      case FilterDialogTreeNode.ELSEFILTER_TYPE:
      {
        disableComparisonOperations();
        disableFeatureOperations();
        disableLogicalOperations();
        break;
      }
      case FilterDialogTreeNode.LOGICAL_NODE_TYPE:
      {
        // no feature id
        disableFeatureOperations();
        // if NOT -> only one child possible
        if( currentNode.getSubType() == FilterDialogTreeNode.LOCICAL_NOT )
        {
          if( children.length == 0 )
          {
            enableComparisonOperations();
            enableLogicalOperations();
          }
          else
          {
            disableComparisonOperations();
            disableLogicalOperations();
          }
        }
        // else (AND,OR)-> unbounded number of children possible
        else
        {
          enableComparisonOperations();
          enableLogicalOperations();
        }
        break;
      }
      case FilterDialogTreeNode.COMPARISON_NODE_TYPE:
      {
        disableComparisonOperations();
        disableFeatureOperations();
        disableLogicalOperations();
        if( FilterDialogTreeNode.isBinaryComparisonType( currentNode.getSubType() ) )
          drawBinaryComparisonOpTypeGroup( currentNode.getData(), currentNode.getSubType() );
        else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_BETWEEN )
          drawPropertyIsBetweenTypeGroup( (BetweenComparisonData)currentNode.getData() );
        else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_LIKE )
          drawPropertyIsLikeTypeGroup( (LikeComparisonData)currentNode.getData() );
        else if( currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_NULL )
          drawPropertyIsNullTypeGroup( (NullComparisonData)currentNode.getData() );
        break;
      }
      case FilterDialogTreeNode.FEATUREID_NODE_TYPE:
      {
        disableComparisonOperations();
        disableFeatureOperations();
        disableLogicalOperations();
        drawFeatureIdTypeGroup( (FeatureIDData)currentNode.getData() );
        break;
      }

      default:
      {
        System.out.println( "Error selectionChanged in FilterDialog" );
      }
      }
    }
  }

  private void disableLogicalOperations()
  {
    logicalCombo.disable();
    logicalButton.setEnabled( false );
  }

  private void enableLogicalOperations()
  {
    logicalCombo.enable();
    logicalButton.setEnabled( true );
  }

  private void disableComparisonOperations()
  {
    comparisonCombo.disable();
    compButton.setEnabled( false );
  }

  private void enableComparisonOperations()
  {
    comparisonCombo.enable();
    compButton.setEnabled( true );
  }

  private void disableFeatureOperations()
  {
    featureIdButton.setEnabled( false );
  }

  private void enableFeatureOperations()
  {
    featureIdButton.setEnabled( true );
  }

  private void disableElseFilter()
  {
    elseFilterButton.setEnabled( false );
  }

  private void enableElseFilter()
  {
    elseFilterButton.setEnabled( true );
  }

  private void createContextMenu( Control menuControl )
  {
    MenuManager menuMgr = new MenuManager( "#PopUp" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new MenuAction( "Delete" ) );
      }
    } );

    Menu menu = menuMgr.createContextMenu( menuControl );
    menuControl.setMenu( menu );
  }

  private void drawFeatureIdTypeGroup( FeatureIDData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    //		 **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( "Configure Filter" );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( "FeatureID" );
    final Text featureIdText = new Text( configureGroup, SWT.BORDER );
    featureIdText
        .setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    featureIdText.setLayoutData( textData );

    if( data != null && data.getFeatureId() != null )
      featureIdText.setText( data.getFeatureId() );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( "set" );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        FeatureIDData fiddata = new FeatureIDData();
        fiddata.setFeatureId( featureIdText.getText() );
        boolean validInput = false;
        try
        {
          validInput = fiddata.verify();
          if( validInput )
          {
            getCurrentNode().setData( fiddata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
            featureIdText.setText( ( (FeatureIDData)getCurrentNode().getData() ).getFeatureId() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );

  }

  private void drawPropertyIsNullTypeGroup( NullComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    //		 **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( "Configure Filter" );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( "Property" );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList labelStringItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      if( !ftp[i].getType().startsWith( "org.deegree.model.geometry." ) )
        labelStringItems.add( ftp[i].getName() );
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = (String)labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( "set" );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        NullComparisonData ncdata = new NullComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          ncdata.setPropertyName( items[index] );
        boolean validInput = false;
        try
        {
          validInput = ncdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( ncdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawBinaryComparisonOpTypeGroup( AbstractData data, final int subType )
  {
    String propertyName = null;
    String literal = null;

    if( data instanceof BinaryComparisonData )
    {
      propertyName = ( (BinaryComparisonData)data ).getPropertyName();
      literal = ( (BinaryComparisonData)data ).getLiteral();
    }
    else if( data instanceof BinaryComparisonNumericData )
    {
      propertyName = ( (BinaryComparisonNumericData)data ).getPropertyName();
      literal = ( (BinaryComparisonNumericData)data ).getLiteral();
    }

    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    //		 **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( "Configure Filter" );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( "Property" );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList labelStringItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      // if only numeric types should be selectable
      if( subType != FilterDialogTreeNode.COMPARISON_EQUALTO )
      {
        if( ftp[i].getType().equalsIgnoreCase( "java.lang.Double" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigInteger" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Byte" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigDecimal" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Float" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Integer" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Long" ) )
          labelStringItems.add( ftp[i].getName() );
        else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Short" ) )
          labelStringItems.add( ftp[i].getName() );
      }
      // any type except for a geometry object
      else
      {
        if( !ftp[i].getType().startsWith( "org.deegree.model.geometry." ) )
          labelStringItems.add( ftp[i].getName() );
      }
    }
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = (String)labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( "Value" );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText
        .setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    if( propertyName != null && propertyName.trim().length() > 0 )
      propertyNameCombo.select( labelStringItems.indexOf( propertyName ) );
    if( literal != null && literal.trim().length() > 0 )
      literalText.setText( literal );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( "set" );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        AbstractData addata = null;
        int index = propertyNameCombo.getSelectionIndex();
        if( subType == FilterDialogTreeNode.COMPARISON_EQUALTO )
        {
          addata = new BinaryComparisonData();
          if( index >= 0 && index < items.length )
            ( (BinaryComparisonData)addata ).setPropertyName( items[index] );
          ( (BinaryComparisonData)addata ).setLiteral( literalText.getText() );
        }
        else
        {
          addata = new BinaryComparisonNumericData();
          if( index >= 0 && index < items.length )
            ( (BinaryComparisonNumericData)addata ).setPropertyName( items[index] );
          ( (BinaryComparisonNumericData)addata ).setLiteral( literalText.getText() );
        }

        boolean validInput = false;
        try
        {
          validInput = addata.verify();
          if( validInput )
          {
            getCurrentNode().setData( addata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
          {
            if( subType == FilterDialogTreeNode.COMPARISON_EQUALTO )
              literalText.setText( ( (BinaryComparisonData)getCurrentNode().getData() )
                  .getLiteral() );
            else
              literalText.setText( ( (BinaryComparisonNumericData)getCurrentNode().getData() )
                  .getLiteral() );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawPropertyIsLikeTypeGroup( LikeComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    //		 **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( "Configure Filter" );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( "Property" );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList labelStringItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      if( !ftp[i].getType().startsWith( "org.deegree.model.geometry." ) )
        labelStringItems.add( ftp[i].getName() );
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = (String)labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );

    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( "Value" );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText
        .setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }
    if( data != null && data.getLiteral() != null )
    {
      literalText.setText( data.getLiteral() );
    }
    if( data == null )
      data = new LikeComparisonData();
    Label wildCard = new Label( configureGroup, SWT.NULL );
    wildCard.setText( "Wildcard: '" + data.getWildCard() + "'" );
    Label otherInfo = new Label( configureGroup, SWT.NULL );
    otherInfo.setText( "SingleChar: '" + data.getSingleChar() + "'  Escape: '"
        + data.getEscapeChar() + "'" );

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( "set" );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        LikeComparisonData lcdata = new LikeComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          lcdata.setPropertyName( items[index] );
        lcdata.setLiteral( literalText.getText() );
        boolean validInput = false;
        try
        {
          validInput = lcdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( lcdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
            literalText.setText( ( (LikeComparisonData)getCurrentNode().getData() ).getLiteral() );
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  private void drawPropertyIsBetweenTypeGroup( BetweenComparisonData data )
  {
    if( innerConfigureComposite != null )
      innerConfigureComposite.dispose();
    innerConfigureComposite = new Composite( globalConfigureComposite, SWT.NULL );
    innerConfigureComposite.setLayout( new GridLayout() );
    GridData formData = new GridData( 224, 127 );
    innerConfigureComposite.setLayoutData( formData );

    //		 **** Configuration
    configureGroup = new Group( innerConfigureComposite, SWT.NULL );
    configureGroup.setLayout( new GridLayout( 2, false ) );
    configureGroup.setText( "Configure Filter" );
    GridData groupCompositeData = new GridData();
    groupCompositeData.widthHint = 200;
    groupCompositeData.heightHint = 100;
    configureGroup.setLayoutData( groupCompositeData );

    Label propertyLabel = new Label( configureGroup, SWT.NULL );
    propertyLabel.setText( "Property" );
    final Combo propertyNameCombo = new Combo( configureGroup, SWT.NULL );
    GridData propertyNameComboData = new GridData( 75, 30 );
    propertyNameCombo.setLayoutData( propertyNameComboData );
    // get all PropertyNames to use for filter
    ArrayList labelStringItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      if( ftp[i].getType().equalsIgnoreCase( "java.lang.Double" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigInteger" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Byte" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigDecimal" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Float" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Integer" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Long" ) )
        labelStringItems.add( ftp[i].getName() );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Short" ) )
        labelStringItems.add( ftp[i].getName() );
    }
    final String[] items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = (String)labelStringItems.get( j );
    propertyNameCombo.setItems( items );
    propertyNameCombo.setText( "..." );
    Label literalLabel = new Label( configureGroup, SWT.NULL );
    literalLabel.setText( "LowerBoundary" );
    final Text literalText = new Text( configureGroup, SWT.BORDER );
    literalText
        .setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData = new GridData();
    textData.widthHint = 90;
    textData.heightHint = 10;
    literalText.setLayoutData( textData );

    Label literalLabel2 = new Label( configureGroup, SWT.NULL );
    literalLabel2.setText( "UpperBoundary" );
    final Text literalText2 = new Text( configureGroup, SWT.BORDER );
    literalText2
        .setBackground( new org.eclipse.swt.graphics.Color( null, new RGB( 255, 255, 255 ) ) );
    GridData textData2 = new GridData();
    textData2.widthHint = 90;
    textData2.heightHint = 10;
    literalText2.setLayoutData( textData2 );

    if( data != null && data.getPropertyName() != null )
    {
      propertyNameCombo.select( labelStringItems.indexOf( data.getPropertyName() ) );
    }
    if( data != null && data.getLower() != null )
    {
      literalText.setText( data.getLower() );
    }
    if( data != null && data.getUpper() != null )
    {
      literalText2.setText( data.getUpper() );
    }

    Button addButton = new Button( configureGroup, SWT.NULL );
    addButton.setText( "set" );
    if( labelStringItems.size() == 0 )
      addButton.setEnabled( false );
    addButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        BetweenComparisonData bcdata = new BetweenComparisonData();
        int index = propertyNameCombo.getSelectionIndex();
        if( index >= 0 && index < items.length )
          bcdata.setPropertyName( items[index] );
        bcdata.setLower( literalText.getText() );
        bcdata.setUpper( literalText2.getText() );
        boolean validInput = false;
        try
        {
          validInput = bcdata.verify();
          if( validInput )
          {
            getCurrentNode().setData( bcdata );
            setFilterInvalid();
          }
          getErrorLabel().setText( "" );
        }
        catch( FilterDialogException e1 )
        {
          getErrorLabel().setText( e1.getError().getFaultCode() );
          if( getCurrentNode().getData() != null )
          {
            literalText.setText( ( (BetweenComparisonData)getCurrentNode().getData() ).getLower() );
            literalText2.setText( ( (BetweenComparisonData)getCurrentNode().getData() ).getUpper() );
          }
        }
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
    } );

    innerConfigureComposite.pack( true );
    globalConfigureComposite.setVisible( true );
  }

  Filter getFilter()
  {
    return returnFilter;
  }

  void setFilterInvalid()
  {
    isValidated = false;
    validateFilterButton.setText( "validate" );
    returnFilter = null;
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == FilterDialogListener.class )
      {
        FilterDialogEvent event = new FilterDialogEvent( this );
        ( (FilterDialogListener)listeners[i + 1] ).filterUpdated( event );
      }
    }
  }

  public Filter getReturnFilter()
  {
    return returnFilter;
  }

  public void setReturnFilter( Filter m_returnFilter )
  {
    this.returnFilter = m_returnFilter;
  }

  public Button getValidateFilterButton()
  {
    return validateFilterButton;
  }

  public void setValidateFilterButton( Button m_validateFilterButton )
  {
    this.validateFilterButton = m_validateFilterButton;
  }

  public Label getErrorLabel()
  {
    return errorLabel;
  }

  public void setErrorLabel( Label m_errorLabel )
  {
    this.errorLabel = m_errorLabel;
  }

  public boolean isValidated()
  {
    return isValidated;
  }

  public void setValidated( boolean m_isValidated )
  {
    this.isValidated = m_isValidated;
  }

  public FilterDialogTreeNode getMRoot()
  {
    return mRoot;
  }

  public void setMRoot( FilterDialogTreeNode root )
  {
    mRoot = root;
  }

  public ComparisonFilterComboPanel getComparisonCombo()
  {
    return comparisonCombo;
  }

  public void setComparisonCombo( ComparisonFilterComboPanel m_comparisonCombo )
  {
    this.comparisonCombo = m_comparisonCombo;
  }

  public TableTreeViewer getM_viewer()
  {
    return m_viewer;
  }

  public void setM_viewer( TableTreeViewer m_m_viewer )
  {
    this.m_viewer = m_m_viewer;
  }

  public FilterDialogTreeNode getCurrentNode()
  {
    return currentNode;
  }

  public void setCurrentNode( FilterDialogTreeNode m_currentNode )
  {
    this.currentNode = m_currentNode;
  }

  public LogicalFilterComboPanel getLogicalCombo()
  {
    return logicalCombo;
  }

  public void setLogicalCombo( LogicalFilterComboPanel m_logicalCombo )
  {
    this.logicalCombo = m_logicalCombo;
  }

  public FeatureType getFeatureType()
  {
    return featureType;
  }

  public void setFeatureType( FeatureType m_featureType )
  {
    this.featureType = m_featureType;
  }

  public Rule getRule()
  {
    return rule;
  }

  public void setRule( Rule m_rule )
  {
    this.rule = m_rule;
  }

  protected class MenuAction extends Action
  {
    public MenuAction( String text )
    {
      super( text );
    }

    public void run()
    {
      getErrorLabel().setText( "" );
      setFilterInvalid();
      IStructuredSelection selection = (IStructuredSelection)getM_viewer().getSelection();

      if( selection != null )
      {
        Object selectedElement = selection.getFirstElement();
        FilterDialogTreeNode node = (FilterDialogTreeNode)selectedElement;
        if( node.getType() == FilterDialogTreeNode.ROOT_TYPE )
        {
          Object[] children = node.getChildren();
          for( int i = 0; i < children.length; i++ )
            node.removeNode( (FilterDialogTreeNode)children[i] );
          setCurrentNode( node );
        }
        else
        {
          FilterDialogTreeNode parent = node.getParent();
          if( parent != null )
            parent.removeNode( node );
          setCurrentNode( parent );
        }
        getM_viewer().setInput( getMRoot() );
        if( getCurrentNode() != null )
          getM_viewer().setSelection( new StructuredSelection( getCurrentNode() ) );
        getM_viewer().expandAll();
      }
    }
  }

}