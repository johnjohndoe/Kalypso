package com.bce.datacenter.kalypso.dcbrowser;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
//import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.tree.TreeNode;
import com.bce.datacenter.ingres.Database;
import com.bce.datacenter.kalypso.Channel;
import com.bce.datacenter.kalypso.Level;
import com.bce.datacenter.kalypso.Timeserie;
//import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.data.TSLink;

/**
 * Timeseries Browser
 *
 * @author schlienger
 */
public class DCBrowser extends JFrame {
	protected final DCChannelTableModel m_tableModel =
		new DCChannelTableModel(Level.getRoot());
	private final DCTreeModel m_treeModel = new DCTreeModel();
	protected final JTable m_table = new JTable(m_tableModel);
	private final JTree m_tree = new JTree(m_treeModel);

	private static DCBrowser instance = null;

	private JButton ok = new JButton(I18n.get("Dia_OK"));
	private JButton cancel = new JButton(I18n.get("Dia_Cancel"));
	private JButton clear = new JButton(I18n.get("Dia_Clear"));

	public static DCBrowser getInstance() {
		if (instance == null)
			instance = new DCBrowser();
		return instance;
	}

	public String getSelectedTimeSeriesKey() {
		int row = m_table.getSelectionModel().getLeadSelectionIndex();
		//System.out.println("Selcted Row: "+row);
		if(row == -1){
			row = 0;
			//System.out.println("Selcted Row: "+row);
		}
		Timeserie timeserie = m_tableModel.getTimeserie(row);
		if(timeserie != null){
		return timeserie.getDataTableName();
		}else{
			return null;
		}
	}

	public TreePath getTreePath(String timeSeriesKey) {
		try {
			List backpath = new ArrayList();

			Timeserie timeserie = Timeserie.findTimeserie(timeSeriesKey);

			Channel channel = timeserie.getChannel();
			backpath.add(channel);

			Level parent = (Level) channel.getOwner();

			while (parent != null) {
				backpath.add(parent);

				parent = (Level) parent.getParentLevel();
			}
			Object path[] = new Object[backpath.size()];
			Object treepath[] = new Object[backpath.size()];

			for (int i = backpath.size() - 1; i >= 0; i--) {
				path[backpath.size() - i - 1] = backpath.get(i);
			}

			int level = 0;
			TreeNode current = (TreeNode) Level.getRoot();
			treepath[level] = current;
			while (level < path.length - 1) {

				int childsize = current.getChildCount();
				for (int i = 0; i < childsize; i++) {
					TreeNode node = current.getChildAt(i);
					if (node.equals(path[level + 1])) {
						level++;
						current = node;
						treepath[level] = node;

						break;
					}
				}
			}

			return new TreePath(treepath);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public void expandTo(String timeSeriesKey) {

		TreePath treepath = getTreePath(timeSeriesKey);
		m_tree.expandPath(treepath);
		m_tree.setSelectionPath(treepath);
		try {
			Timeserie timeserie = Timeserie.findTimeserie(timeSeriesKey);
			int pos = timeserie.getChannel().getChildPos(timeserie);
			m_table.getSelectionModel().setSelectionInterval(pos, pos);
		} catch (Exception e) {
			e.printStackTrace();
		}
		m_table.validate();
		m_tree.validate();
		validate();
	}

	public TSLink getSelectedNode() {

		if (instance.getSelectedTimeSeriesKey() != null)
		{
		String text=getTreePath(instance.getSelectedTimeSeriesKey()).toString();
		text=text.replaceAll(",","/");			
		text=text.replaceAll("\\[","");			
		text=text.replaceAll("\\]","");			
			return new TSLink(
				text+ ","
					+ instance.getSelectedTimeSeriesKey());
		}
		else
			return new TSLink(null);
	}

	public void setActionListener(
		ActionListener okListener,
		ActionListener cancelListener,
		ActionListener clearListener) {
		ok.addActionListener(okListener);
		cancel.addActionListener(cancelListener);
		clear.addActionListener(clearListener);
	}

	/**
	 * Creates a new DCBrowser object.
	 */
	private DCBrowser() {
		// configure tree
		m_tree.getSelectionModel().setSelectionMode(
			TreeSelectionModel.SINGLE_TREE_SELECTION);
		m_tree.putClientProperty("JTree.lineStyle", "Angled");
		m_tree.setShowsRootHandles(true);
		m_tree.setEditable(false);
		m_tree.addTreeSelectionListener(new DirSelectionListener());

		m_table.getSelectionModel().setSelectionMode(
			ListSelectionModel.SINGLE_SELECTION);
		m_table.addMouseListener(new EltSelectionListener());

		JPanel panel = new JPanel(new BorderLayout());
		panel.add(
			new JSplitPane(
				JSplitPane.HORIZONTAL_SPLIT,
				new JScrollPane(m_tree),
				new JScrollPane(m_table)),
			BorderLayout.CENTER);
		this.getContentPane().add(panel);

		//ButtonPanel
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
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		setSize(500, 400);
		setTitle(I18n.get("Title_Zeitreihen-Browser"));
	}

	public static void main(String[] args) {
		// here we establish the connection with the database...
		try {
			Database.init(
				"ca.edbc.jdbc.EdbcDriver",
				"jdbc:edbc://PC083:LP7/BCE_PC083::dbabwb2/INGRES",
				"ingres",
				"ingres");

			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}

		DCBrowser dcb = new DCBrowser();

		dcb.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		dcb.setVisible(true);
	}

	/**
	 * get the tree node at the end of the path
	 *
	 * @param path a tree path
	 *
	 * @return ?
	 */
	public TreeNode pathToLevel(TreePath path) {
		return (TreeNode) (path.getLastPathComponent());
	}

	/**
	 * Selection listener
	 *
	 * @author schlienger
	 */
	class DirSelectionListener implements TreeSelectionListener {
		public void valueChanged(TreeSelectionEvent event) {
			Object o = pathToLevel(event.getPath());
			if (o != null && o instanceof Channel)
				m_tableModel.setCurrentChannel((Channel) o);
			if (o != null && o instanceof Level)
				m_tableModel.setCurrentLevel((Level) o);
		}
	}

	/**
	 * 
	 * @author schlienger
	 *
	 */
	class EltSelectionListener extends JPopupMenu implements MouseListener {
		/**
		 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent e) {
			checkPopup(e);
		}

		/**
		 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent e) {
			checkPopup(e);
		}

		/**
		 * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent e) {
			checkPopup(e);
		}

		/**
		 * Checks if this is a popup trigger and shows popup menu
		 *
		 * @param e -
		 */
		private void checkPopup(MouseEvent e) {
			this.removeAll();

			if (e.isPopupTrigger()) {
				int pos = m_table.getSelectedRow();
				/*			
								Level level = m_tableModel.getCurrentLevel();
							
								List objects = level.getObjects();
							
								if( pos >= 0 && pos < objects.size() )
								{
									Channel channel =  (Channel) objects.get( pos );
									
									List tss = channel.getTimeseries();
									
									Iterator it = tss.iterator();
									
									while( it.hasNext() )
									{
										add( new TimeserieAction( (Timeserie)it.next() ) );
									}
								}
				*/
				show(m_table, e.getX(), e.getY());

			}
		}

		/**
		 * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
		 */
		public void mouseEntered(MouseEvent e) {
			// nothing
		}

		/**
		 * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
		 */
		public void mouseExited(MouseEvent e) {
			// nothing			
		}
	}

	/**
	 * 
	 * @author schlienger
	 */
	private static class TimeserieAction extends AbstractAction {
		private final String m_table;

		/**
		 * @param name
		 */
		public TimeserieAction(Timeserie t) {
			super(t.toString());

			m_table = t.getDataTableName();

			putValue(Action.NAME, t.toString());
		}

		/**
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			// TODO retrieve the table name from here
			System.out.println(m_table);
		}
	}
}
