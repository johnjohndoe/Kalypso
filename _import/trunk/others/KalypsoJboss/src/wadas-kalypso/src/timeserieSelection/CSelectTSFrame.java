package timeserieSelection;
import de.tuhh.wb.javagis.data.TSLink;

import java.util.Enumeration;
import javax.swing.*;

import javax.swing.event.*;

import javax.swing.tree.*;

import java.awt.BorderLayout;

import de.tuhh.wb.javagis.view.ViewManager;

import java.awt.event.ActionEvent;

import java.awt.event.ActionListener;



import datacenter.persistent.*;

import datacenter.tree.*;

import datacenter.zeitreihen.*;



import javax.swing.JMenu;

import javax.swing.JMenuItem;

import javax.swing.JMenuBar;

import de.tuhh.wb.javagis.tools.I18n;



/**

 * @author ingres

 *

 */

public class CSelectTSFrame extends JDialog implements ActionListener

{

    private JTree m_tree = null;

    private DefaultTreeModel m_model = null;

    private CTSStruct selectedTS=null;
	
	private JButton ok = new JButton(I18n.get("Dia_OK"));
    private	JButton cancel = new JButton(I18n.get("Dia_Cancel"));
	private JButton clear = new JButton(I18n.get("Dia_Clear"));

    public CSelectTSFrame()

	{

	    //super("time series browser",true,false,true,true);
		super();
	;
		getContentPane().setLayout(new BorderLayout());
		
		JPanel buttonPanel = new JPanel();

		ActionListener okListener1 = new ActionListener() {
           	 public void actionPerformed(ActionEvent e) {
				hide();
				}
		};
		ok.addActionListener(okListener1);
		buttonPanel.add(ok);
		
		ActionListener cancelListener1 = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				hide();
			}
		};
		cancel.addActionListener(cancelListener1);
				ActionListener clearListener1 = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				hide();
			}
		};
		clear.addActionListener(clearListener1);
		buttonPanel.add(cancel);
		buttonPanel.add(clear);
		getContentPane().add(buttonPanel,BorderLayout.SOUTH);

	    // create the root node

		//DefaultMutableTreeNode root = new DefaultMutableTreeNode( new CTSTreeNode("root", ""), true );

		

		// create the tree model

		m_model = new DefaultTreeModel(null);

		

		m_tree = new JTree( m_model );

		

		// configure tree

		m_tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

		m_tree.putClientProperty("JTree.lineStyle", "Angled");

		m_tree.setShowsRootHandles( true );

		m_tree.setEditable( false );

		

		m_tree.addTreeSelectionListener(new DirSelectionListener());

		

		// add tree to display

		JScrollPane scrollPane = new JScrollPane();

		scrollPane.getViewport().add( m_tree );

		this.getContentPane().add(scrollPane,BorderLayout.CENTER);

		

		// fill in the tree with our database elements





		fillInTree(Level.GetRootLevel(), null);



		JMenu jView = new JMenu (I18n.get("TimeSeries_view"));



		JMenuItem mi;

		mi = new JMenuItem(I18n.get("TimeSeries_updateTree"));

		mi.setToolTipText("  ");

		mi.setActionCommand("updateTree");

		mi.addActionListener(this);

		jView.add(mi);

		JMenuBar menubar= new JMenuBar();

		menubar.add(jView);

		this.setJMenuBar(menubar);



		setSize(300,400);
		setTitle(I18n.get("TimeSeries_FrameName"));
		

		//ViewManager.desktop.add(this);

	};

		



	/** get the tree node at the end of the path

	 *

	 */

	public DefaultMutableTreeNode getTreeNode(TreePath path)

	{

		return (DefaultMutableTreeNode)(path.getLastPathComponent());

	};

	

	/** convenience function returning the root node

	 *

	 */

	public DefaultMutableTreeNode getRootNode()

	{

		return (DefaultMutableTreeNode)m_model.getRoot();

	};

	

	

	/** get the timeserie info node from a tree node

	 *

	 */

        public CTSTreeNode getTSNode(DefaultMutableTreeNode node)

	{

		if( node == null )

			return null;

		

		Object obj = node.getUserObject();

		

		if( obj instanceof CTSTreeNode )

			return (CTSTreeNode)obj;

		else

			return null;

	};

	public void setActionListener(ActionListener okListener, ActionListener cancelListener, ActionListener clearListener){
		ok.addActionListener(okListener);
		cancel.addActionListener(cancelListener);
		clear.addActionListener(clearListener);
	}
	

    public TSLink getSelectedNode()

    {

	if(this.getSelectedTS()!=null)

	    return new TSLink(this.getSelectedTSPath()+","+this.getSelectedTS().m_tableName);

	else
	    return new TSLink(null);
    }




	private void fillInTree( Level level, DefaultMutableTreeNode parent )

	{

		if( level == null )

			return;

		

		// create a new node

		DefaultMutableTreeNode node = new DefaultMutableTreeNode( new CTSTreeNode(level.GetLevelName(), "") );

		

		Level[] childs = level.GetChildLevels();

		

		for(int i=0; i < childs.length; i++)

		{

			fillInTree( childs[i], node );

		}

		

		Container[] objects = level.GetObjects();

		

		for(int i=0; i < objects.length; i++)

		{

			addObject( objects[i], node );

		}



		if( parent == null )

			m_model.setRoot(node);

		else

			// finally add it to given tree node

			parent.add( node );

	};

	

	private void addObject(Container object, DefaultMutableTreeNode parent )

	{

		if( object == null )

			return;

		

		Channel[] channels = object.GetChannels();

		

		if( channels.length > 0 )

		{

			// create a new node

			DefaultMutableTreeNode node = new DefaultMutableTreeNode( new CTSTreeNode(object.GetObjectName(), "") );



			for(int i=0; i < channels.length; i++)

			{

				addChannel(channels[i], node);

			}

		

			parent.add( node );

		}

	}

	

	private void addChannel(Channel channel, DefaultMutableTreeNode parent )

	{

		if( channel == null )

			return;



		// create a new node

		DefaultMutableTreeNode cnode = new DefaultMutableTreeNode( new CTSTreeNode(channel.GetName() + "(channel)", "") );

		parent.add( cnode );



		DefaultMutableTreeNode node = new DefaultMutableTreeNode( new CTSTreeNode(channel.GetName() + "(org)", channel.GetOriginalName()) );

		cnode.add( node );

		node = new DefaultMutableTreeNode( new CTSTreeNode(channel.GetName() + "(wrk)", channel.GetWorkName()) );

		cnode.add( node );



		Derivation[] ders = channel.GetDerivations();



		for(int i=0; i < ders.length; i++)

		{

			node = new DefaultMutableTreeNode( new CTSTreeNode(ders[i].GetName(), ders[i].GetTableName()), false );



			cnode.add( node );

		}



	}

	

    public void actionPerformed(ActionEvent e)

    {

	String command=e.getActionCommand();

	if("updateTree".equals(command))

	{

	    fillInTree(Level.GetRootLevel(), null);

	}

    }

    

    class DirSelectionListener implements TreeSelectionListener

    {

	public void valueChanged(TreeSelectionEvent event)

	{

	    DefaultMutableTreeNode node = getTreeNode(event.getPath());

		    CTSTreeNode tsNode = getTSNode( node );

		    

		    if( tsNode != null )

			{

			    System.out.println( tsNode.getTimeserieInfo().toString() );

			    if(!"".equals(tsNode.getTimeserieInfo().m_tableName))

				selectedTS=tsNode.getTimeserieInfo();

			    else

				selectedTS=null;

			}

		    else

			selectedTS=null;

		};

	}; // class DirSelectionListener

    

    public String getSelectedTSPath()

    {

	String path="";

	if(selectedTS!=null && m_tree.getSelectionPath()!=null)

	    {

		Object nodes[]=m_tree.getSelectionPath().getPath();

		for(int i=0;i<nodes.length;i++)

		    path=path+"/"+nodes[i].toString();

	    }

	return path;

    }



    public CTSStruct getSelectedTS()

    {

	return selectedTS;

    }

	public void prepareWindow(TSLink tsLink)
	{
	        m_tree.clearSelection();
		//		fillInTree(Level.GetRootLevel(), null);
		m_tree.collapsePath(new TreePath(m_model.getPathToRoot(getRootNode())));
		if(tsLink==null)
			return;
		String code=tsLink.getCode();
		if(code==null)
			return;
		TreeNode tsNode=getPathForCode(getRootNode(),code);
		if(tsNode!=null)
		{
			TreePath path=new TreePath(m_model.getPathToRoot(tsNode));
			System.out.println("TreePath:"+path.toString());
			m_tree.expandPath(path);
			m_tree.setSelectionPath(path);
		}
	}
	
	public TreeNode getPathForCode(TreeNode node,String code)
	{
	  if(node.isLeaf())
		{
	     CTSTreeNode ctsNode=getTSNode((DefaultMutableTreeNode)node);
			if(ctsNode!=null)
			{
				if(ctsNode.getTimeserieInfo().m_tableName.equals(code))
					return node;
				
			}
		}
		else // no leaf
		{
	     Enumeration en=node.children();
	     while(en.hasMoreElements())
			{
			 	TreeNode childNode=(TreeNode)en.nextElement();
				TreeNode resultNode=getPathForCode(childNode,code);
			    if(resultNode!=null)
					return resultNode;
			}
		
		}
		return null;
	}
}

