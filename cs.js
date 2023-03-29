const crypto = require("crypto");
const path = require("path");
const fs = require("fs");

// Place ALL files from Kaggle in datasets/fitbit_kaggle/
// Place first month of MTurk (Zenodo) datasets in datasets/fitbit_mturk_3.12.16-4.11.16
// Place second month of MTurk (Zenodo) datasets in datasets/fitbit_mturk_4.12.16-5.12.16


// Snippet from Welling Guzman (https://wellingguzman.com/notes/node-checksum)
const getChecksum = (source) => {
    return new Promise((resolve, reject) => {
        const hash = crypto.createHash('md5');
        const input = fs.createReadStream(source);

        input.on('error', reject);
        input.on('data', (chunk) => hash.update(chunk));
        input.on('close', () => resolve(hash.digest('hex')));
    })
}
//

const KAGGLE_DIR = path.join(__dirname, "datasets/fitbit_kaggle/");
const SUPPLEMENT_DIR = path.join(__dirname, "datasets/fitbit_mturk_3.12.16-4.11.16");
const MTURK_DIR = path.join(__dirname, "datasets/fitbit_mturk_4.12.16-5.12.16");

const KAGGLE_FILES = fs.readdirSync(KAGGLE_DIR);
const SUPPLEMENT_FILES = fs.readdirSync(SUPPLEMENT_DIR);
const MTURK_FILES = fs.readdirSync(MTURK_DIR);

const KAGGLE_SET = new Set(KAGGLE_FILES);
const MTURK_MONTH_1_SET =new Set(SUPPLEMENT_FILES);
const MTURK_MONTH_2_SET = new Set(MTURK_FILES);
const MTURK_MONTHS_SET = new Set([...SUPPLEMENT_FILES, ...MTURK_FILES]);
const KAGGLE_MTURK_SET = new Set([...KAGGLE_FILES, ...MTURK_FILES]);

console.log(`Kaggle folder contains ${KAGGLE_FILES.length} file(s)`);
console.log(`Mturk month 1 folder contains ${SUPPLEMENT_FILES.length} file(s)`);
console.log(`Mturk month 2 folder contains ${MTURK_FILES.length} file(s)`);

// 1. Compare Kaggle files with second month of 
// the Mturk dataset  >>
console.log("\nKaggle vs. Mturk Month 2");

let present_files = [];
let kaggle_missing_files = [];
let mturk_month_2_missing_files = [];

KAGGLE_MTURK_SET.forEach((f) => {
    if (KAGGLE_SET.has(f) && !MTURK_MONTH_2_SET.has(f)) {
        mturk_month_2_missing_files.push(f);
    } else if (!KAGGLE_SET.has(f) && MTURK_MONTH_2_SET.has(f)) {
        kaggle_missing_files.push(f);
    } else {
        present_files.push(f);
    }
})

if (present_files[0]) {
    console.log(`Kaggle data and Mturk month 2 folders share ${present_files.length} file(s):`)
    for (f of present_files) console.log(` - ${f}`);
}

if (kaggle_missing_files[0]) {
    console.log(`Kaggle is missing ${kaggle_missing_files.length} file(s):`);
    for (f of kaggle_missing_files) console.log(` - ${f}`);
}

if (mturk_month_2_missing_files[0]) {
    console.log(`Mturk month 2 is missing ${mturk_month_2_missing_files.length} file(s):`);
    for (f of mturk_month_2_missing_files) console.log(` - ${f}`);
}

// 2. Compare the first month of the Mturk dataset
// with the second month of the Mturk dataset.
console.log("\nMturk month 1 Vs. Mturk month 2");

present_files = [];
let mturk_month_1_missing_files = [];
mturk_month_2_missing_files = [];

MTURK_MONTHS_SET.forEach((f) => {
    if (MTURK_MONTH_1_SET.has(f) && !MTURK_MONTH_2_SET.has(f)) {
        mturk_month_2_missing_files.push(f);
    } else if (!MTURK_MONTH_1_SET.has(f) && MTURK_MONTH_2_SET.has(f)) {
        mturk_month_1_missing_files.push(f);
    } else {
        present_files.push(f);
    }
})

if (present_files[0]) {
    console.log(`Mturk month 1 and Mturk month 2 folders share ${present_files.length} file(s):`)
    for (f of present_files) console.log(` - ${f}`);
}

if (mturk_month_1_missing_files[0]) {
    console.log(`Mturk month 1 is missing ${mturk_month_1_missing_files.length} file(s):`);
    for (f of mturk_month_1_missing_files) console.log(` - ${f}`);
}

if (mturk_month_2_missing_files[0]) {
    console.log(`Mturk month 2 is missing ${mturk_month_2_missing_files.length} file(s):`);
    for (f of mturk_month_2_missing_files) console.log(` - ${f}`);
}

console.log("\nKaggle vs. Mturk month 2 checksum comparison")
const resolver = new Promise(async () => {
    let alterationDiscovered = false;

    for (i = 0; i < KAGGLE_FILES.length; i++) {

        if (KAGGLE_FILES[i] === MTURK_FILES[i]) {
            let kaggleFile = path.join(KAGGLE_DIR, KAGGLE_FILES[i]);
            let mturkFile = path.join(MTURK_DIR, MTURK_FILES[i]);
    
            let kaggleChecksum = await getChecksum(kaggleFile);
            let mturkChecksum = await getChecksum(mturkFile);
    
            if (kaggleChecksum !== mturkChecksum) {
                alterationDiscovered = true;
                console.log("These two do not appear to match:");
                console.log(`Kaggle Indice ${i} checksum: ${kaggleChecksum} (${kaggleFile})`);
                console.log(`Mturk Indice ${i} checksum: ${mturkChecksum} (${mturkFile})`);
            }
        }
        else {
            console.log(`File mismatch: File of index ${i} (${KAGGLE_FILES[i]}) does not share the same name as paired file ${MTURK_FILES[i]}`)
            process.exit(-1);
        }
    }
    
    if (!alterationDiscovered) {
        console.log("Success! Files appear to be unaltered.")
    }
})

resolver
    .then(() => console.log("Exiting!"))
    .catch(e => console.error(e));