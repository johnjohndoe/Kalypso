package timeserieSelection;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import de.tuhh.wb.javagis.view.ViewManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import datacenter.persistent.*;
import datacenter.tree.*;
import datacenter.zeitreihen.*;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;


/**
 * @author ingres
 *
 */
public class CSelectTSFrame extends JInternalFrame implements ActionListener
{
    private JTree m_tree = null;
    private DefaultTreeModel m_model = null;
    private CTSStruct selectedTS=null;
    
    public CSelectTSFrame()
	{
	    super("time series browser",true,false,true,true);

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
		this.getContentPane().add(scrollPane);
		
		// fill in the tree with our database elements


		fillInTree(Level.GetRootLevel(), null);

		JMenu jView = new JMenu ("view");

		JMenuItem mi;
		mi = new JMenuItem("update tree");
		mi.setToolTipText("  ");
		mi.setActionCommand("updateTree");
		mi.addActionListener(this);
		jView.add(mi);
		JMenuBar menubar= new JMenuBar();
		menubar.add(jView);
		this.setJMenuBar(menubar);	

		setSize(300,400);
		ViewManager.desktop.add(this);
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
	if(selectedTS!=null)
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
}
