/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


import java.util.ArrayList;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.services.wfs.filterencoding.Expression;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterConstructionException;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree_impl.services.wfs.filterencoding.AbstractFilter;
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
import org.kalypso.editor.styleeditor.dialogs.filterencoding.BoundaryExpression;
import org.kalypso.editor.styleeditor.panels.ComparisonFilterComboPanel;
import org.kalypso.editor.styleeditor.panels.LogicalFilterComboPanel;



public class FilterDialog extends Dialog implements ISelectionChangedListener{

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
				
	private Composite globalConfigureComposite = null;	
	private Composite innerConfigureComposite = null;
	
	private Button validateFilterButton = null;
	private boolean isValidated = false;
	
	private Filter returnFilter = null;

	public FilterDialog(Shell parent, FeatureType featureType, Filter filter) {
		super(parent);
		this.featureType = featureType;
		if(filter != null && filter instanceof AbstractFilter)
		{			
			FilterDialogTreeNode filterTree = parseFilterIntoTree((AbstractFilter)filter);
			if(filterTree != null)
			{
				mRoot = new FilterDialogTreeNode("Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE);
				mRoot.addNode(filterTree);
			}
		}
	}

	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText("Filter Dialog");
		shell.setSize(500,300);
	}

	protected Control createDialogArea(Composite parent) {
		returnFilter = null;
		Composite composite = (Composite) super.createDialogArea(parent);
		composite.setSize(500,300);
		composite.setLayout(new GridLayout(1,true));		
		composite.layout();				
		applyDialogFont(composite);

		// **** FIRST ROW - FUNCTION MENU
		Composite functionComposite = new Composite(composite, SWT.NULL);
		functionComposite.setLayout(new GridLayout(7,false));
		Label functionLabel = new Label(functionComposite,SWT.NULL);
		GridData labelData = new GridData();
		labelData.widthHint = 40;
		functionLabel.setLayoutData(labelData);		
		functionLabel.setText("Options:");			
		logicalCombo = new LogicalFilterComboPanel(functionComposite);		
		logicalButton = new Button(functionComposite, SWT.NULL);
		logicalButton.setText("Add");
		logicalButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {								
				FilterDialogTreeNode childNode = new FilterDialogTreeNode(logicalCombo.getSelectionName(logicalCombo.getSelection()),FilterDialogTreeNode.LOGICAL_NODE_TYPE);
				currentNode.addNode(childNode);
				m_viewer.expandAll();
				if(childNode != null)
					m_viewer.setSelection(new StructuredSelection(childNode));								
				m_viewer.refresh(true);	
				setFilterInvalid();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		comparisonCombo = new ComparisonFilterComboPanel(functionComposite);
		compButton = new Button(functionComposite, SWT.NULL);
		compButton.setText("Add");
		compButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {								
				FilterDialogTreeNode childNode = new FilterDialogTreeNode(comparisonCombo.getSelectionName(comparisonCombo.getSelection()),FilterDialogTreeNode.COMPARISON_NODE_TYPE);
				currentNode.addNode(childNode);
				m_viewer.expandAll();
				if(currentNode != null)
					m_viewer.setSelection(new StructuredSelection(currentNode));								
				m_viewer.refresh(true);	
				setFilterInvalid();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		featureIdButton = new Button(functionComposite, SWT.NULL);
		featureIdButton.setText("Add Feat");
		featureIdButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {								
				FilterDialogTreeNode childNode = new FilterDialogTreeNode("FT_ID",FilterDialogTreeNode.FEATUREID_NODE_TYPE);
				currentNode.addNode(childNode);			
				m_viewer.expandAll();
				if(currentNode != null)
					m_viewer.setSelection(new StructuredSelection(currentNode));								
				m_viewer.refresh(true);
				setFilterInvalid();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
		
		// **** SECOND ROW - TREE 
		Composite secondRowComposite = new Composite(composite, SWT.NULL);
		secondRowComposite.setLayout(new GridLayout(2,true));
					
		final TableTree tree = new TableTree(secondRowComposite, SWT.SINGLE |SWT.FULL_SELECTION |SWT.H_SCROLL);		
		GridData tableTreeData = new GridData();
		tableTreeData.widthHint = 224;
		tableTreeData.heightHint = 97;	
		tree.setLayoutData(tableTreeData);					
		m_viewer = new TableTreeViewer(tree);	
		m_viewer.addSelectionChangedListener(this);		
		m_viewer.setContentProvider(new FilterDialogTreeContentProvider());
		m_viewer.setLabelProvider(new FilterDialogLabelProvider());		
		if(mRoot == null)
			mRoot = new FilterDialogTreeNode("Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE);		
		m_viewer.setInput(mRoot);		
		m_viewer.setSelection(new StructuredSelection(mRoot.getParent()));
		m_viewer.expandAll();
		createContextMenu(m_viewer.getControl(), mRoot);

		
		globalConfigureComposite = new Composite(secondRowComposite,SWT.NULL);
		globalConfigureComposite.setLayout(new GridLayout());
		GridData globalConfigureCompositeData = new GridData();
		globalConfigureCompositeData.widthHint = 224;
		globalConfigureCompositeData.heightHint = 127;
		globalConfigureComposite.setLayoutData(globalConfigureCompositeData);
		
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		
		// **** THIRD ROW - SAVE FILTER
		Composite thirdRowComposite = new Composite(composite, SWT.NULL);
		thirdRowComposite.setLayout(new GridLayout(1,true));
		validateFilterButton = new Button(thirdRowComposite, SWT.NULL);		
		validateFilterButton.setText("validate");
		validateFilterButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {										
				Object[] children = ((FilterDialogTreeNode)mRoot.getChildren()[0]).getChildren();
				if(children.length != 0)
				{					
					// generate Filter
					if(isValidated)
					{	
						validateFilterButton.setText("generate");
						returnFilter = generateFilter(children);						
						System.out.println(returnFilter.toXML().toString());
					}
					else
					{
						if(validateFilter(children))
						{
							validateFilterButton.setText("generate");
							isValidated = true;
						}							
					}
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		
		return composite;
	}	
	
	private FilterDialogTreeNode parseFilterIntoTree(AbstractFilter filter)
	{
		if(filter instanceof FeatureFilter)
		{
			FeatureFilter featureFilter = (FeatureFilter) filter;
			ArrayList featureIds = featureFilter.getFeatureIds();			
			FilterDialogTreeNode tmpNode = null;
			FeatureIDData tmpData = null;
			for(int i=0; i<featureIds.size(); i++)
			{
				tmpNode = new FilterDialogTreeNode("FT_ID",FilterDialogTreeNode.FEATUREID_NODE_TYPE);
				tmpData = new FeatureIDData();
				tmpData.setFeatureId(featureIds.get(i).toString());	
				tmpNode.setData(tmpData);				
			}
			return tmpNode;
		}
		else if(filter instanceof ComplexFilter)
		{
			ComplexFilter complexFilter = (ComplexFilter) filter;
			Operation operation = complexFilter.getOperation();
			if(operation instanceof LogicalOperation)
			{
				LogicalOperation logOp = (LogicalOperation) operation;
				FilterDialogTreeNode tmpNode = null;				
				switch(logOp.getOperatorId())
				{
					case OperationDefines.AND:
					{
						tmpNode = new FilterDialogTreeNode("AND", FilterDialogTreeNode.LOGICAL_NODE_TYPE);
						break;
					}
					case OperationDefines.OR:
					{
						tmpNode = new FilterDialogTreeNode("OR", FilterDialogTreeNode.LOGICAL_NODE_TYPE);
						break;
					}
					case OperationDefines.NOT:
					{
						tmpNode = new FilterDialogTreeNode("NOT", FilterDialogTreeNode.LOGICAL_NODE_TYPE);
						break;
					}										
				}										
				// ArrayList with Operations (logic, comp, spatial)
				ArrayList arguments = logOp.getArguments();
				for(int i=0; i<arguments.size(); i++)
				{
					ComplexFilter tmpFilter = new ComplexFilter((Operation)arguments.get(i));
					tmpNode.addNode(parseFilterIntoTree(tmpFilter));
				}
				return tmpNode;
			}
			else if(operation instanceof ComparisonOperation)
			{
				ComparisonOperation compOp = (ComparisonOperation) operation;
				FilterDialogTreeNode tmpNode = null;				
				switch(compOp.getOperatorId())
				{
					case OperationDefines.PROPERTYISLIKE:
					{
						tmpNode = new FilterDialogTreeNode("LIKE", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsLikeOperation isLikeOp = (PropertyIsLikeOperation)compOp;
						LikeComparisonData data = new LikeComparisonData();						
						data.setLiteral(isLikeOp.getLiteral().getValue());
						data.setPropertyName(isLikeOp.getPropertyName().getValue());
						data.setEscapeChar(isLikeOp.getEscapeChar());
						data.setSingleChar(isLikeOp.getSingleChar());
						data.setWildCard(isLikeOp.getWildCard());						
						tmpNode.setData(data);
						break;
					}
					case OperationDefines.PROPERTYISNULL:
					{
						tmpNode = new FilterDialogTreeNode("NULL", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsNullOperation isNullOp = (PropertyIsNullOperation) compOp;
						NullComparisonData data = new NullComparisonData();
						Expression exp = isNullOp.getExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						tmpNode.setData(data);
						break;
					}
					case OperationDefines.PROPERTYISBETWEEN:
					{
						tmpNode = new FilterDialogTreeNode("BETWEEN", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsBetweenOperation isBetweenOp = (PropertyIsBetweenOperation) compOp;
						BetweenComparisonData data = new BetweenComparisonData();						
						data.setPropertyName(isBetweenOp.getPropertyName().getValue());						
						data.setLower(((BoundaryExpression)isBetweenOp.getLowerBoundary()).getValue());
						data.setUpper(((BoundaryExpression)isBetweenOp.getUpperBoundary()).getValue());
						tmpNode.setData(data);
						break;
					}	
					case OperationDefines.PROPERTYISEQUALTO:
					{
						tmpNode = new FilterDialogTreeNode("EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
						BinaryComparisonData data = new BinaryComparisonData();
						Expression exp = isCompOp.getFirstExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						exp = isCompOp.getSecondExpression();
						if(exp instanceof Literal)
							data.setLiteral(((Literal)exp).getValue());						
						tmpNode.setData(data);											
						break;						
					}
					case OperationDefines.PROPERTYISLESSTHAN:
					{
						tmpNode = new FilterDialogTreeNode("LESS_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
						BinaryComparisonData data = new BinaryComparisonData();
						Expression exp = isCompOp.getFirstExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						exp = isCompOp.getSecondExpression();
						if(exp instanceof Literal)
							data.setLiteral(((Literal)exp).getValue());						
						tmpNode.setData(data);											
						break;						
					}
					case OperationDefines.PROPERTYISGREATERTHAN:
					{
						tmpNode = new FilterDialogTreeNode("GREATER_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
						BinaryComparisonData data = new BinaryComparisonData();
						Expression exp = isCompOp.getFirstExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						exp = isCompOp.getSecondExpression();
						if(exp instanceof Literal)
							data.setLiteral(((Literal)exp).getValue());						
						tmpNode.setData(data);											
						break;						
					}
					case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
					{
						tmpNode = new FilterDialogTreeNode("LESS_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
						BinaryComparisonData data = new BinaryComparisonData();
						Expression exp = isCompOp.getFirstExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						exp = isCompOp.getSecondExpression();
						if(exp instanceof Literal)
							data.setLiteral(((Literal)exp).getValue());						
						tmpNode.setData(data);											
						break;						
					}	
					case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
					{
						tmpNode = new FilterDialogTreeNode("GREATER_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE);
						PropertyIsCOMPOperation isCompOp = (PropertyIsCOMPOperation) compOp;
						BinaryComparisonData data = new BinaryComparisonData();
						Expression exp = isCompOp.getFirstExpression();
						if(exp instanceof PropertyName)
							data.setPropertyName(((PropertyName)exp).getValue());
						exp = isCompOp.getSecondExpression();
						if(exp instanceof Literal)
							data.setLiteral(((Literal)exp).getValue());						
						tmpNode.setData(data);											
						break;
					}						
				}														
				return tmpNode;
			}								
		}
		return null;
	}
	
	private Filter generateFilter(Object[] children)
	{							
		if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE)
		{
			FeatureFilter filter = new FeatureFilter();
			for(int i=0; i<children.length; i++)
			{
				FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[i];
				if(currentChild.getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE)
				{
					FeatureIDData data = (FeatureIDData) currentChild.getData();
					filter.addFeatureId(new FeatureId(data.getFeatureId()));
				}				
			}
			return filter;
		}
		else if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE)
		{
			FilterDialogTreeNode child = (FilterDialogTreeNode)children[0]; 
			
			switch(child.getSubType())
			{
				case FilterDialogTreeNode.COMPARISON_BETWEEN:
				{
					BetweenComparisonData data = (BetweenComparisonData) child.getData();
					BoundaryExpression upperBoundary = new BoundaryExpression(data.getUpper());
					BoundaryExpression lowerBoundary = new BoundaryExpression(data.getLower());
					PropertyName propertyName = new PropertyName(data.getPropertyName());
					PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation(propertyName,lowerBoundary, upperBoundary);
					return new ComplexFilter(operation);
				}
				case FilterDialogTreeNode.COMPARISON_LIKE:
				{
					LikeComparisonData data = (LikeComparisonData) child.getData();
					PropertyName propertyName = new PropertyName(data.getPropertyName());
					Literal literal = new Literal(data.getLiteral());				
					PropertyIsLikeOperation operation = new PropertyIsLikeOperation(propertyName,literal,data.getWildCard(),data.getSingleChar(),data.getEscapeChar());
					return new ComplexFilter(operation);	
				}
				case FilterDialogTreeNode.COMPARISON_NULL:
				{
					NullComparisonData data = (NullComparisonData)child.getData();
					PropertyName propertyName = new PropertyName(data.getPropertyName());
					PropertyIsNullOperation operation = new PropertyIsNullOperation(propertyName);
					return new ComplexFilter(operation);
				}
				case FilterDialogTreeNode.COMPARISON_EQUALTO:
				case FilterDialogTreeNode.COMPARISON_LESSTHAN:
				case FilterDialogTreeNode.COMPARISON_GREATERTHAN:
				case FilterDialogTreeNode.COMPARISON_LESSTHANOREQUALTO:
				case FilterDialogTreeNode.COMPARISON_GREATERTHANOREQUALTO:
				{
					BinaryComparisonData data = (BinaryComparisonData) child.getData();
					PropertyName propertyName = new PropertyName(data.getPropertyName());
					Literal literal = new Literal(data.getLiteral());						
					PropertyIsCOMPOperation operation = new PropertyIsCOMPOperation(child.getSubType(),propertyName,literal);
					return new ComplexFilter(operation);
				}
			}			
		}
		else if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE)
		{
			FilterDialogTreeNode child = (FilterDialogTreeNode)children[0];
			if(child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT)
			{				
				ArrayList arguments = new ArrayList();
				Object[] innerElement = {((FilterDialogTreeNode) child.getChildren()[0])};
				Filter filter = generateFilter(innerElement);
				if(filter instanceof ComplexFilter)
				{
					arguments.add(((ComplexFilter)filter).getOperation());
				}
				try {
					LogicalOperation operation = new LogicalOperation(OperationDefines.NOT,arguments);
					return new ComplexFilter(operation);
				} catch (FilterConstructionException e) {					
					e.printStackTrace();
				}				
			}
			else if(child.getSubType() == FilterDialogTreeNode.LOCICAL_AND)
			{
				ArrayList arguments = new ArrayList();
				for(int i=0; i<child.getChildren().length; i++)
				{
					Object[] innerElement = {((FilterDialogTreeNode) child.getChildren()[i])};
					Filter filter = generateFilter(innerElement);
					if(filter instanceof ComplexFilter)
					{
						arguments.add(((ComplexFilter)filter).getOperation());
					}
				}					
				try {
					LogicalOperation operation = new LogicalOperation(OperationDefines.AND,arguments);
					return new ComplexFilter(operation);
				} catch (FilterConstructionException e) {					
					e.printStackTrace();
				}									
			}																				
			else if(child.getSubType() == FilterDialogTreeNode.LOCICAL_OR)
			{
				ArrayList arguments = new ArrayList();
				for(int i=0; i<child.getChildren().length; i++)
				{
					Object[] innerElement = {((FilterDialogTreeNode) child.getChildren()[i])};
					Filter filter = generateFilter(innerElement);
					if(filter instanceof ComplexFilter)
					{
						arguments.add(((ComplexFilter)filter).getOperation());
					}
				}					
				try {
					LogicalOperation operation = new LogicalOperation(OperationDefines.OR,arguments);
					return new ComplexFilter(operation);
				} catch (FilterConstructionException e) {					
					e.printStackTrace();
				}
			}
			
		}
		return null;
	}
	
	// parses the Filter to check whether valid structure
	private boolean validateFilter(Object[] children)
	{			
		// element has to have at least one child
		if(children.length == 0)
			return false;
		
		if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE)
		{
			// check every FeatureIDElement whether valid
			for(int i=0; i<children.length; i++)
			{
				FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[i];
				// if one is not valid
				if(!currentChild.validate())
				{
					return false;
				}
			}
			return true;
		}
		else if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.COMPARISON_NODE_TYPE)
		{
			FilterDialogTreeNode currentChild = (FilterDialogTreeNode) children[0];
			if(!currentChild.validate())
			{
				return false;
			}
			return true;					
		}
		else if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.LOGICAL_NODE_TYPE)
		{						
			FilterDialogTreeNode child = (FilterDialogTreeNode)children[0];			
			if(child.getSubType() == FilterDialogTreeNode.LOCICAL_NOT)
			{	
				// NOT needs to have exactly one child
				if(child.getChildren().length != 1)
					return false;
				if(!((FilterDialogTreeNode) child.getChildren()[0]).validate())
					return false;
				return true;						
			}
			else if(child.getSubType() == FilterDialogTreeNode.LOCICAL_AND)
			{
				// AND needs to have more than 1 child
				if(child.getChildren().length < 2)
					return false;
				for(int i=0; i<child.getChildren().length; i++)
				{
					if(!((FilterDialogTreeNode) child.getChildren()[i]).validate())
						return false;
				}				
				return true;								
			}																				
			else if(child.getSubType() == FilterDialogTreeNode.LOCICAL_OR)
			{
				// OR needs to have more than 1 child
				if(child.getChildren().length < 2)
					return false;
				for(int i=0; i<child.getChildren().length; i++)
				{
					if(!((FilterDialogTreeNode) child.getChildren()[i]).validate())
						return false;
				}				
				return true;
			}		
		}
		return false;
	}	
	
	
	/* (non-Javadoc)
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */	
	public void selectionChanged(SelectionChangedEvent event) 
	{					
		TableTreeViewer ttv = (TableTreeViewer) event.getSource();			
		IStructuredSelection s = (IStructuredSelection)ttv.getSelection();		
		if(s.getFirstElement() instanceof FilterDialogTreeNode)
		{					
			currentNode = (FilterDialogTreeNode) s.getFirstElement();					
			Object[] children =  currentNode.getChildren();
			if(globalConfigureComposite != null && !globalConfigureComposite.isDisposed())
				globalConfigureComposite.setVisible(false);
			switch(currentNode.getType())
			{
				case FilterDialogTreeNode.ROOT_TYPE:
				{		
					// no first element -> all options available
					if(children.length == 0)
					{
						enableComparisonOperations();
						enableFeatureOperations();
						enableLogicalOperations();
					}
					// first element: featureId -> only featureId available
					else if(((FilterDialogTreeNode)children[0]).getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE)
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
				case FilterDialogTreeNode.LOGICAL_NODE_TYPE:
				{
					// no feature id
					disableFeatureOperations();
					// if NOT -> only one child possible
					if(currentNode.getSubType() == FilterDialogTreeNode.LOCICAL_NOT)
					{
						if(children.length == 0)
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
					if(FilterDialogTreeNode.isBinaryComparisonType(currentNode.getSubType()))
						drawBinaryComparisonOpTypeGroup((BinaryComparisonData) currentNode.getData());
					else if(currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_BETWEEN)
						drawPropertyIsBetweenTypeGroup((BetweenComparisonData) currentNode.getData());
					else if(currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_LIKE)
						drawPropertyIsLikeTypeGroup((LikeComparisonData) currentNode.getData());
					else if(currentNode.getSubType() == FilterDialogTreeNode.COMPARISON_NULL)
						drawPropertyIsNullTypeGroup((NullComparisonData) currentNode.getData());					
					break;					
				}
				case FilterDialogTreeNode.FEATUREID_NODE_TYPE:
				{
					disableComparisonOperations();
					disableFeatureOperations();
					disableLogicalOperations();					
					drawFeatureIdTypeGroup((FeatureIDData) currentNode.getData());					
					break;
				}
				
				default:
				{
					System.out.println("Error");
				}
			}						
		}				
	}
	
	private void disableLogicalOperations()
	{
		logicalCombo.disable();
		logicalButton.setEnabled(false);
	}
	
	private void enableLogicalOperations()
	{
		logicalCombo.enable();
		logicalButton.setEnabled(true);
	}	
	
	private void disableComparisonOperations()
	{
		comparisonCombo.disable();
		compButton.setEnabled(false);
	}	
	
	private void enableComparisonOperations()
	{
		comparisonCombo.enable();
		compButton.setEnabled(true);
	}	
	
	private void disableFeatureOperations()
	{
		featureIdButton.setEnabled(false);		
	}
	
	private void enableFeatureOperations()
	{
		featureIdButton.setEnabled(true);		
	}
	
	

	private void createContextMenu(Control menuControl, final FilterDialogTreeNode node) 
	{
		MenuManager menuMgr = new MenuManager("#PopUp");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() 
		{
			public void menuAboutToShow(IMenuManager manager) 
			{
				manager.add(new MenuAction("Delete"));
			}
		});

		Menu menu = menuMgr.createContextMenu(menuControl);
		menuControl.setMenu(menu);
	}
	
	private void drawFeatureIdTypeGroup(FeatureIDData data)
	{	
		if(innerConfigureComposite != null)
			innerConfigureComposite.dispose();				
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		innerConfigureComposite.setLayout(new GridLayout());
		GridData formData = new GridData(224,127);
		innerConfigureComposite.setLayoutData(formData);
		
		//		 **** Configuration			
		configureGroup = new Group(innerConfigureComposite,SWT.NULL);
		configureGroup.setLayout(new GridLayout (2,false));
		configureGroup.setText("Configure Filter");
		GridData groupCompositeData = new GridData();
		groupCompositeData.widthHint = 200;
		groupCompositeData.heightHint = 100;	
		configureGroup.setLayoutData(groupCompositeData);			
		
		Label propertyLabel = new Label(configureGroup,SWT.NULL);
		propertyLabel.setText("FeatureID");
		final Text featureIdText = new Text(configureGroup, SWT.BORDER);		
		featureIdText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));
		GridData textData = new GridData();
		textData.widthHint = 90;
		textData.heightHint = 10;
		featureIdText.setLayoutData(textData);
		if(data != null && data.getFeatureId() != null)
			featureIdText.setText(data.getFeatureId());
		
		Button addButton = new Button(configureGroup,SWT.NULL);
		addButton.setText("Add to Filter");
		addButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				FeatureIDData data = new FeatureIDData();				
				data.setFeatureId(featureIdText.getText());
				if(data.verify())
				{
					currentNode.setData(data);
					setFilterInvalid();
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		
		innerConfigureComposite.pack(true);
		globalConfigureComposite.setVisible(true);
		
	}
	
	private void drawPropertyIsNullTypeGroup(NullComparisonData data)
	{	
		if(innerConfigureComposite != null)
			innerConfigureComposite.dispose();				
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		innerConfigureComposite.setLayout(new GridLayout());
		GridData formData = new GridData(224,127);
		innerConfigureComposite.setLayoutData(formData);
		
		//		 **** Configuration			
		configureGroup = new Group(innerConfigureComposite,SWT.NULL);
		configureGroup.setLayout(new GridLayout (2,false));
		configureGroup.setText("Configure Filter");
		GridData groupCompositeData = new GridData();
		groupCompositeData.widthHint = 200;
		groupCompositeData.heightHint = 100;	
		configureGroup.setLayoutData(groupCompositeData);			
		
		Label propertyLabel = new Label(configureGroup,SWT.NULL);
		propertyLabel.setText("Property");
		final Combo propertyNameCombo = new Combo(configureGroup,SWT.NULL);
		// get all PropertyNames to use for filter
		ArrayList labelStringItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(!ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			labelStringItems.add(ftp[i].getName()); 
    	final String[] items = new String[labelStringItems.size()];
    	for(int j=0; j<items.length; j++)
    		items[j] = (String)labelStringItems.get(j);    	
    	propertyNameCombo.setItems(items);
    	propertyNameCombo.setText("...");
		
    	if(data != null && data.getPropertyName() != null)
		{
			propertyNameCombo.select(labelStringItems.indexOf(data.getPropertyName()));
		}	
		
		Button addButton = new Button(configureGroup,SWT.NULL);
		addButton.setText("Add to Filter");
		addButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				NullComparisonData data = new NullComparisonData();
				int index = propertyNameCombo.getSelectionIndex();
				if(index >=0 && index <items.length)
					data.setPropertyName(items[index]);		
				if(data.verify())	
				{
					currentNode.setData(data);	
					setFilterInvalid();
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
		
		innerConfigureComposite.pack(true);
		globalConfigureComposite.setVisible(true);
	}	
	
	private void drawBinaryComparisonOpTypeGroup(BinaryComparisonData data)
	{		
		if(innerConfigureComposite != null)
			innerConfigureComposite.dispose();				
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		innerConfigureComposite.setLayout(new GridLayout());
		GridData formData = new GridData(224,127);
		innerConfigureComposite.setLayoutData(formData);
		
		//		 **** Configuration			
		configureGroup = new Group(innerConfigureComposite,SWT.NULL);
		configureGroup.setLayout(new GridLayout (2,false));
		configureGroup.setText("Configure Filter");
		GridData groupCompositeData = new GridData();
		groupCompositeData.widthHint = 200;
		groupCompositeData.heightHint = 100;	
		configureGroup.setLayoutData(groupCompositeData);				
		
		Label propertyLabel = new Label(configureGroup,SWT.NULL);
		propertyLabel.setText("Property");
		final Combo propertyNameCombo = new Combo(configureGroup,SWT.NULL);
		// get all PropertyNames to use for filter
		ArrayList labelStringItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(!ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			labelStringItems.add(ftp[i].getName()); 
    	final String[] items = new String[labelStringItems.size()];
    	for(int j=0; j<items.length; j++)
    		items[j] = (String)labelStringItems.get(j);    	
    	propertyNameCombo.setItems(items);
    	propertyNameCombo.setText("...");    		
    	
		Label literalLabel = new Label(configureGroup,SWT.NULL);
		literalLabel.setText("Value");
		final Text literalText = new Text(configureGroup, SWT.BORDER);		
		literalText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));
		GridData textData = new GridData();
		textData.widthHint = 90;
		textData.heightHint = 10;
		literalText.setLayoutData(textData);
		
		if(data != null && data.getPropertyName() != null)
		{
			propertyNameCombo.select(labelStringItems.indexOf(data.getPropertyName()));
		}		
		if(data != null && data.getLiteral() != null)
		{
			literalText.setText(data.getLiteral());			
		}		
		
		Button addButton = new Button(configureGroup,SWT.NULL);
		addButton.setText("Add to Filter");
		addButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				BinaryComparisonData data = new BinaryComparisonData();
				int index = propertyNameCombo.getSelectionIndex();
				if(index >=0 && index <items.length)
					data.setPropertyName(items[index]);												  
				data.setLiteral(literalText.getText());
				if(data.verify())
				{
					currentNode.setData(data);	
					setFilterInvalid();
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});			
		
		innerConfigureComposite.pack(true);
		globalConfigureComposite.setVisible(true);
	}	
	
	private void drawPropertyIsLikeTypeGroup(LikeComparisonData data)
	{		
		if(innerConfigureComposite != null)
			innerConfigureComposite.dispose();				
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		innerConfigureComposite.setLayout(new GridLayout());
		GridData formData = new GridData(224,127);
		innerConfigureComposite.setLayoutData(formData);
		
		//		 **** Configuration			
		configureGroup = new Group(innerConfigureComposite,SWT.NULL);
		configureGroup.setLayout(new GridLayout (2,false));
		configureGroup.setText("Configure Filter");
		GridData groupCompositeData = new GridData();
		groupCompositeData.widthHint = 200;
		groupCompositeData.heightHint = 100;	
		configureGroup.setLayoutData(groupCompositeData);				
		
		Label propertyLabel = new Label(configureGroup,SWT.NULL);
		propertyLabel.setText("Property");
		final Combo propertyNameCombo = new Combo(configureGroup,SWT.NULL);
		// get all PropertyNames to use for filter
		ArrayList labelStringItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(!ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			labelStringItems.add(ftp[i].getName()); 
    	final String[] items = new String[labelStringItems.size()];
    	for(int j=0; j<items.length; j++)
    		items[j] = (String)labelStringItems.get(j);    	
    	propertyNameCombo.setItems(items);
    	propertyNameCombo.setText("...");
    	
		Label literalLabel = new Label(configureGroup,SWT.NULL);
		literalLabel.setText("Value");
		final Text literalText = new Text(configureGroup, SWT.BORDER);		
		literalText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));
		GridData textData = new GridData();
		textData.widthHint = 90;
		textData.heightHint = 10;
		literalText.setLayoutData(textData);
		
		if(data != null && data.getPropertyName() != null)
		{
			propertyNameCombo.select(labelStringItems.indexOf(data.getPropertyName()));
		}		
		if(data != null && data.getLiteral() != null)
		{
			literalText.setText(data.getLiteral());			
		}	
		
		Button addButton = new Button(configureGroup,SWT.NULL);
		addButton.setText("Add to Filter");
		addButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				LikeComparisonData data = new LikeComparisonData();				
				int index = propertyNameCombo.getSelectionIndex();
				if(index >=0 && index <items.length)
					data.setPropertyName(items[index]);											
				data.setLiteral(literalText.getText());
				if(data.verify())
				{
					currentNode.setData(data);
					setFilterInvalid();
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});					
		
		innerConfigureComposite.pack(true);
		globalConfigureComposite.setVisible(true);
	}	
	
	private void drawPropertyIsBetweenTypeGroup(BetweenComparisonData data)
	{		
		if(innerConfigureComposite != null)
			innerConfigureComposite.dispose();				
		innerConfigureComposite = new Composite(globalConfigureComposite,SWT.NULL);
		innerConfigureComposite.setLayout(new GridLayout());
		GridData formData = new GridData(224,127);
		innerConfigureComposite.setLayoutData(formData);
		
		//		 **** Configuration			
		configureGroup = new Group(innerConfigureComposite,SWT.NULL);
		configureGroup.setLayout(new GridLayout (2,false));
		configureGroup.setText("Configure Filter");
		GridData groupCompositeData = new GridData();
		groupCompositeData.widthHint = 200;
		groupCompositeData.heightHint = 100;	
		configureGroup.setLayoutData(groupCompositeData);				
		
		Label propertyLabel = new Label(configureGroup,SWT.NULL);
		propertyLabel.setText("Property");
		final Combo propertyNameCombo = new Combo(configureGroup,SWT.NULL);
		// get all PropertyNames to use for filter
		ArrayList labelStringItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(!ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			labelStringItems.add(ftp[i].getName()); 
    	final String[] items = new String[labelStringItems.size()];
    	for(int j=0; j<items.length; j++)
    		items[j] = (String)labelStringItems.get(j);    	
    	propertyNameCombo.setItems(items);
    	propertyNameCombo.setText("...");
    	
		Label literalLabel = new Label(configureGroup,SWT.NULL);
		literalLabel.setText("LowerBoundary");
		final Text literalText = new Text(configureGroup, SWT.BORDER);		
		literalText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));
		GridData textData = new GridData();
		textData.widthHint = 90;
		textData.heightHint = 10;
		literalText.setLayoutData(textData);

		Label literalLabel2 = new Label(configureGroup,SWT.NULL);
		literalLabel2.setText("UpperBoundary");
		final Text literalText2 = new Text(configureGroup, SWT.BORDER);		
		literalText2.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));
		GridData textData2 = new GridData();
		textData2.widthHint = 90;
		textData2.heightHint = 10;
		literalText2.setLayoutData(textData2);
		
		if(data != null && data.getPropertyName() != null)
		{
			propertyNameCombo.select(labelStringItems.indexOf(data.getPropertyName()));
		}		
		if(data != null && data.getLower() != null)
		{
			literalText.setText(data.getLower());			
		}
		if(data != null && data.getUpper() != null)
		{
			literalText2.setText(data.getUpper());			
		}		
		
		Button addButton = new Button(configureGroup,SWT.NULL);
		addButton.setText("Add to Filter");
		addButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				BetweenComparisonData data = new BetweenComparisonData();
				int index = propertyNameCombo.getSelectionIndex();
				if(index >=0 && index <items.length)
					data.setPropertyName(items[index]);											
				data.setLower(literalText.getText());
				data.setUpper(literalText2.getText());				
				if(data.verify())
				{
					currentNode.setData(data);
					setFilterInvalid();					
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});					
		
		innerConfigureComposite.pack(true);
		globalConfigureComposite.setVisible(true);
	}	
	
	public Filter getFilter()
	{
		return returnFilter;
	}
	
	private void setFilterInvalid()
	{
		isValidated = false;
		validateFilterButton.setText("validate");
		returnFilter = null;
	}
	protected class MenuAction extends Action {		
		public MenuAction(String text) {
			super(text);			
		}

		public void run() {					

			IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();

			if (selection != null)
			{				
				Object selectedElement = selection.getFirstElement();
				FilterDialogTreeNode node = (FilterDialogTreeNode) selectedElement;					
				if(node.getType() == FilterDialogTreeNode.ROOT_TYPE)
				{
					Object[] children = node.getChildren();
					for(int i=0; i<children.length; i++)
						node.removeNode((FilterDialogTreeNode)children[i]);
					currentNode = node;
				}
				else
				{
					FilterDialogTreeNode parent = node.getParent();
					if(parent != null)
						parent.removeNode(node);	
					currentNode = parent;					
				}
				m_viewer.setInput(mRoot);				
				if(currentNode != null)
					m_viewer.setSelection(new StructuredSelection(currentNode));
				m_viewer.expandAll();
			}
		}
	}

}