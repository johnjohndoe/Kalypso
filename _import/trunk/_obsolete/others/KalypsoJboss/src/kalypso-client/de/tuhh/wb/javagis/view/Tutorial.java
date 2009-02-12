package de.tuhh.wb.javagis.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.net.URL;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeSelectionModel;

public class Tutorial extends JFrame {
    private JEditorPane htmlPane;
    private static boolean DEBUG = false;
    private URL helpURL;

    public Tutorial() {
        super("User Manual");

        //Create the nodes.
        DefaultMutableTreeNode top = new DefaultMutableTreeNode("Tutorial");
        createNodes(top);

        //Create a tree SINGLE_SELECTION_MODULE.
        final JTree tree = new JTree(top);
        tree.getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);


        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
        tree.setCellRenderer(renderer);

        //Listen for when the selection changes.
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent e) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode)
                                   tree.getLastSelectedPathComponent();

                if (node == null) return;

                Object nodeInfo = node.getUserObject();
                if (node.isLeaf()) {
                    BookInfo book = (BookInfo)nodeInfo;
                    displayURL(book.bookURL);
                    if (DEBUG) {
                        System.out.print(book.bookURL + ":  \n    ");
                    }
                } else {
                    displayURL(helpURL);
                }
                if (DEBUG) {
                    System.out.println(nodeInfo.toString());
                }
            }
        });

        //Create the scroll pane and add the tree to it.
        JScrollPane treeView = new JScrollPane(tree);

        //Create the HTML viewing pane.
        htmlPane = new JEditorPane();
        htmlPane.setEditable(false);
        initHelp();
        JScrollPane htmlView = new JScrollPane(htmlPane);

        //Add the scroll panes to a split pane.
        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setLeftComponent(treeView);
        splitPane.setRightComponent(htmlView);
        Dimension minimumSize = new Dimension(100, 50);
        htmlView.setMinimumSize(minimumSize);
        treeView.setMinimumSize(minimumSize);
        splitPane.setDividerLocation(100);
        splitPane.setDividerSize(5);
        splitPane.setPreferredSize(new Dimension(500, 300));

        //Add the split pane to this frame.
        getContentPane().add(splitPane, BorderLayout.CENTER);
        setSize(1200,1200);
        pack();
        setVisible(true);
    }

    private class BookInfo {
        public String bookName;
        public URL bookURL;
        public String prefix = "file:"
                               + System.getProperty("user.dir")
                               + System.getProperty("file.separator")
                               + "tutorial"
                               + System.getProperty("file.separator");
        public BookInfo(String book, String filename) {
            bookName = book;
            try {
                bookURL = new URL(prefix + filename);
            } catch (java.net.MalformedURLException exc) {
                System.err.println("Error "
                                   + "bad URL: " + bookURL);
                bookURL = null;
            }
        }

        public String toString() {
            return bookName;
        }
    }

    private void initHelp() {
        String s = null;
        try {
            s = "file:"
                + System.getProperty("user.dir")
                + System.getProperty("file.separator")
                + "tutorial"
               + System.getProperty("file.separator")
               + "tutorial.html";
            if (DEBUG) {
                System.out.println("Help URL is " + s);
            }
            helpURL = new URL(s);
            displayURL(helpURL);
        } catch (Exception e) {
            System.err.println("Sorry, not possible to create: " + s);
        }
    }

    private void displayURL(URL url) {
        try {
            htmlPane.setPage(url);
        } catch (IOException e) {
            System.err.println("Attempted to read a bad URL: " + url);
        }
    }

    private void createNodes(DefaultMutableTreeNode top) {
        DefaultMutableTreeNode category = null;
        DefaultMutableTreeNode book = null;

        category = new DefaultMutableTreeNode("Introduction ");
        top.add(category);

        //Background Information
       book = new DefaultMutableTreeNode(new BookInfo
                    ("General Information",
                    "BackgroundInfo.html"));
        category.add(book);

        category = new DefaultMutableTreeNode("Data Structure and Versioning ");
        top.add(category);

        //Data Structure
               book = new DefaultMutableTreeNode(new BookInfo
                   ("Data Structure and Versioning",
                    "DataStructure.html"));
        category.add(book);


        category = new DefaultMutableTreeNode("Kalypso RRM- Getting Started ");
        top.add(category);


        //Open Project
        book = new DefaultMutableTreeNode(new BookInfo
            ("Start/End Program",
             "openProject.html"));
        category.add(book);

        //Project Tree
       book = new DefaultMutableTreeNode(new BookInfo
                    ("Managing Project Tree",
                    "ProjectTree.html"));
        category.add(book);

        //Model Data
        book = new DefaultMutableTreeNode(new BookInfo
            ("Model Data-Visualisation/Parameters",
             "ModelData.html"));
        category.add(book);

        //Control Data
        book = new DefaultMutableTreeNode(new BookInfo
            ("Control Data/Simulation Case",
             "ControlData.html"));
        category.add(book);

        category = new DefaultMutableTreeNode("Simulation");
       top.add(category);

       //Input Time-series
       book = new DefaultMutableTreeNode(new BookInfo
           ("Input Time-series",
           "InputT.html"));
        category.add(book);

        //Output Time-series
              book = new DefaultMutableTreeNode(new BookInfo
                  ("Output Time-series",
                  "OutputResults.html"));
        category.add(book);

        category = new DefaultMutableTreeNode("Initial Conditions");
       top.add(category);

       //Parameters
              book = new DefaultMutableTreeNode(new BookInfo
                  ("Initial Conditions-Parameters",
                  "SoilMoisture.html"));
        category.add(book);


       category = new DefaultMutableTreeNode("Calibration");
       top.add(category);

       //Work flow calibration
              book = new DefaultMutableTreeNode(new BookInfo
                  ("Workflow/Parameters",
                  "Calibration.html"));
        category.add(book);


       category = new DefaultMutableTreeNode("Simulation of Flood Events with FLOMATIS Forecast System");
      top.add(category);

        //Preparation of Data for Simualtion
             book = new DefaultMutableTreeNode(new BookInfo
                 ("Preparation of Data for Simulation",
                 "PreparationOfData.html"));
        category.add(book);

        //Forecast
            book = new DefaultMutableTreeNode(new BookInfo
                ("Forecast Simulation",
                "Forecast.html"));
       category.add(book);

       category = new DefaultMutableTreeNode("Forecast Toolbox of FLOMATIS Forecast Model ");
       top.add(category);

       //Forecast Toolbox
            book = new DefaultMutableTreeNode(new BookInfo
                ("Main Features",
                "ForecastTool.html"));
       category.add(book);
    }

    public static void main(String[] args) {
        JFrame frame = new Tutorial();

        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
frame.setSize(1200,1200);
        frame.pack();
        frame.setVisible(true);

    }
}
